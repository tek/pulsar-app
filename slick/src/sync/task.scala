package tryp
package slick
package sync

import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._

import scalaz._, Scalaz._
import ScalazGlobals._
import scalaz.std.string.{stringInstance => zStringInstance}

import cats.data.Xor._

import _root_.slick.jdbc.JdbcProfile

import Codec._
import schema.Action

import SyncResult._

final class ObjectIdTraversableOps(params: Traversable[ObjectId])
{
  def paramList = params map(_.stringify) mkString("(", ",", ")")
}

trait ToObjectIdTraversableOps
{
  implicit def ToObjectIdTraversableOps(params: Traversable[ObjectId]) =
    new ObjectIdTraversableOps(params)
}

case class BackendSyncTask[
A <: SyncModel,
B <: ModelTable[A],
Mapper <: BackendMapper[A]: Decoder: Encoder
](query: SyncQueryI[A, B, Mapper], rest: RestAdapter)
(implicit ec: EC, meta: ModelMetadata[A, _], profile: JdbcProfile)
extends ToObjectIdTraversableOps
{
  import profile.api._
  import query.pendingActionsSchema.Exports._

  val path = rest.request.tip

  def process: ResultsAction = {
    Log.d(s"configuring sync of $query")
    val sent = pendingActionsFor(pendingActionsKey).flatMap {
      _ map send getOrElse pendingActionsKey.syncNoPending.wrapDbioSeq
    }
    val fetcher = Fetcher()
    val flat = for {
      s <- sent
      f <- fetcher.run
    } yield s ++ f
    val embed = meta.embeddedFields map { attr =>
      for {
        sub <- embedded(attr)
        ids <- fetcher.embeddedIds(attr)
      } yield sub ++ ids
    }
    (flat :: embed).suml
  }

  def embedded(attr: EmbeddedAttrMetadataI): ResultsAction = {
    query.all.flatMap { _
      .map(m => rest / m.id)
      .map(r => attr.query.sync(r, attr.query.name))
      .toList
      .suml
    }
  }

  def send(attr: PendingActionSet) = Sender(attr).run

  def pendingActionsKey = query.path

  def pendingActionsFor(name: String) = PendingActionSet.attr(name)

  trait Syncer
  {
    def up: Boolean

    def name = query.path

    def errorWrap[A](error: Any => AnyAction[A])(
      request: => String Xor String)(callback: String => AnyAction[A])
    : AnyAction[A] = {
        suspendedDbio {
          Try(request) match {
            case util.Success(Right(result)) => callback(result)
            case util.Success(Left(err)) => error(err)
            case util.Failure(e @ AuthError(_)) => throw e
            case util.Failure(err) => error(err)
          }
        }
    }

    def errorWrap1 = errorWrap[SyncResult](flatError) _

    def errorWrapList =
      errorWrap[Traversable[SyncResult]](e => flatError(e).wrapDbioSeq) _

    def flatError(e: Any) = {
      path.syncFail(e.toString, up)
    }
  }

  case class Sender(set: PendingActionSet)
  extends Syncer
  {
    def up = true

    def run = {
      for {
        u <- updates
        a <- additions
        d <- deletions
      } yield u ++ a ++ d
    }

    def updates = {
      set.updatesWithoutAdditions flatMap(withJson) flatMap {
        _ map {
          case (u @ Update.e(target, _), data) =>
            errorWrap1 { rest / target body(data) put() } { _ =>
              query.completeSync(u) >> set.model.updateSuccess(data)
            }
        } toDbioSeq
      }
    }

    def additions = {
      set.additions.all flatMap(withJson) flatMap {
        _ map {
          case (a @ Addition.e(target, _), data) =>
            errorWrap1 { rest body(data) post() } { result =>
              query.ids.! flatMap { ids =>
                query.completeSync(a) >> set.model.addSuccess(data)
              }
            }
        } toDbioSeq
      }
    }

    def deletions = {
      set.deletions.all flatMap {
        _ map {
            case d @ Deletion.e(target, _) =>
              errorWrap1 { rest / target delete() } { _ =>
                query.completeDeletion(d) >> set.model.deleteSuccess(target)
              }
        } toDbioSeq
      }
    }

    def withJson(actions: Seq[Action]) = {
      query.jsonForIds(actions.targets) map(a => actions.zip(a.toSeq))
    }
  }

  case class Fetcher()
  extends Syncer
  {
    def up = false

    /* Fetch all remote ids for the current model
     * create difference to local ids
     * fetch data for the missing ids
     * FIXME specify date of last sync or hashes to get updates
     *
     */
    def run = {
      errorWrapList(rest get())(syncFromJson)
      // errorWrapList(rest ids()) { json =>
      //   json.decodeEither[List[ObjectId]] match {
      //     case Right(remoteIds) =>
      //       fetchDiff(remoteIds)
      //     case Left(error) =>
      //       name.fetch(s"ids: $error\n$json")
      //         .failure
      //         .wrapDbioSeq
      //   }
      // }
    }

    def fetchDiff(remoteIds: Seq[ObjectId]) = {
      query.ids.!
        .map(_.toSet)
        .map(remoteIds.toSet.diff)
        .map(_.paramList)
        .map(q => Map("id" -> q))
        .map(rest.params)
        .map(_.get())
        .flatMap(errorWrapList(_)(syncFromJson))
    }

    /* update embedded ids (assoc table) of 'attr' in A records
     * this ensures that the records' assoc table entries point to the updated
     * embedded records that are fetched when calling SyncTask.embedded
     */
    def embeddedIds(attr: EmbeddedAttrMetadataI) = {
      query.all flatMap { records =>
        records map { container: Model =>
          errorWrap1 { rest attrIds(container.id, attr.query.name) } {
            json =>
            decode[List[ObjectId]](json) match {
              case Right(newIds) =>
                updateIds(container, attr.query.name, newIds) >>
                EmbeddedIdStatus(container, attr.query.name, newIds)
                  .success.toDbio
              case Left(error) =>
                EmbeddedIdStatus(container, attr.query.name, s"$error\n$json")
                  .success.toDbio
            }
          }
        } toDbioSeq
      }
    }

    def syncFromJson(json: String): ResultsAction = {
      decode[List[Mapper]](json) match {
        case Right(mappers) =>
          mappers.map(syncFromMapper).toDbioSeq
            .flatMap { added =>
              deleteFromSync(mappers) map { ids =>
                if (ids.isEmpty) added
                else added.toList :+ SimpleSyncStatus(
                  name, s"deleted ${ids.toList.show}", up).success
              }
            }
        case Left(error) =>
          name.fetch(s"$error\n$json")
            .failure
            .point[AnyAction]
            .wrapDbioSeq
      }
    }

    def deleteFromSync(mappers: List[Mapper]) = {
      for {
        ids <- query.ids.!
        delete = ids.toSet &~ mappers.map(_.id).toSet
        _ <- query.deleteByIdsUnrecorded(delete)
      } yield delete
    }

    def syncFromMapper(mapper: Mapper) = {
      MapperSyncer(query, mapper).sync()
        .flatMap(o => query.updateAssocs(o, mapper))
        .status(FetchSyncStatus(name, mapper.asJson.spaces2))
    }

    def updateIds(container: Model, attr: String, ids: List[ObjectId]) = {
      container
        .adapter(attr, false)
        .toDbio(s"no adapter for '$attr' in $container")
        .flatMap(_.replace(ids.toSet))
    }
  }
}

case class BackendSync(exclude: List[String] = Nil)
{
  def apply(schema: SyncSchema)
  (implicit ec: EC, rest: RestClient) = {
      schema.syncMetadata.tables.values
        .filterNot { a => exclude.contains(a.query.path) }
        .map(_.query.sync(RestAdapter(rest)))
        .toList
        .suml
  }
}

case class AuthError(msg: String)
extends java.lang.RuntimeException(msg)
