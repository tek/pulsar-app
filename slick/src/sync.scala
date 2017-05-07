package tryp.slick

import scalaz._, Scalaz._

import tryp.app._

@ModelsImpl(SlickTransformer)
trait SyncSlick
extends Slick
{
  implicit class SyncModelOps(m: ModelSpec)
  extends ModelOps(m)
  {
    lazy val mapperFields = {
      attrs ++ mapperForeignKeys ++ mapperAssocs :+ idColumn
    }

    lazy val mapperForeignKeys = res.refSingles

    lazy val mapperAssocs = res.refLists

    lazy val mapperFieldStrings = mapperFields.map(_.name.toString)

    def attrsWithId = attrs ++ foreignKeys :+ idColumn

    override def queryType = tq"SyncQuery[$tpe, $tableType, $mapperType]"

    override def queryExtra = {
      super.queryExtra ::: codecJson :: handleMapper ::: modelMetadata ::
        pendingActions :: Nil
    }

    override def modelExtra = super.modelExtra ++ jsonAccessors

    override def extra = backendMapper ::: super.extra

    override def modelBases = tq"tryp.slick.sync.SyncModel" :: super.modelBases

    def embeddedData = embeddedAssocs map(_.metadata)

    def modelMetadata = {
      q"""
      val modelMetadata = new tryp.slick.sync.ModelMetadata[$tpe, profile.type]
      {
        def embeddedFields = List(..$embeddedData)
      }
      """
    }

    def handleMapper = {
      val mt = mapperType
      val assocUpdates = mapperAssocs map { f =>
        q"obj.${f.noRecAdapter}.replace(mapper.${f.term})"
      }
      List(
        q"""
        type Mapper = $mapperType
        """,
        q"""
        def mapperJson = $mapperTerm.codecJson
        """,
        q"""
        val Mapper = $mapperTerm
        """,
        q"""
        def updateAssocs(obj: $tpe, mapper: $mt)($iec) = {
          DBIO.seq(..$assocUpdates)
        }
        """
      )
    }

    def codecJson = {
      val att = attrsWithId map { a =>
        (a.name, q"obj.${a.colName}".right: TermPath)
      }
      def instance(extra: List[(String, TermPath)]) = {
        val (names, values) = (extra ++ att).unzip
        val codec = TermName(s"casecodec${values.length}")
        q"""
        $codec(
          {
            (..${attrsWithId map(_.ctorAscription)}) =>
              $term(..${attrsWithId map(_.ctorAssign)})
          },
          { obj: $tpe => Some((..$values)) }
          )(..$names)
        """
      }
      val ctor = if (assocs.isEmpty || true)
        q"${instance(Nil)}"
      else {
        val assTerms = assocs map(_.term.suffix("Ids").left: TermPath)
        val ass = assocs map(_.name) zip(assTerms)
        val assGen = assocs map(_.term) zip(assTerms) map { case (a, t) =>
          fq"$t <- obj.$a.ids"
        }
        q"for(..$assGen) yield ${instance(ass)}"
      }
      q"def codecJson: CodecJson[$tpe] = $ctor"
    }

    // FIXME must inject assoc ids here
    // skip embedded ids, sync separately (already done?)
    def jsonAccessors = {
      val basic = TermName("simpleJson")
      val ctor = if (refAssocs.isEmpty) q"$basic.toDbio"
      else {
        val assTerms = refAssocs map(_.term.suffix("Ids").left: TermPath)
        val ass = refAssocs map(_.name) zip(assTerms)
        val assGen = refAssocs map(_.term) zip(assTerms) map { case (a, t) =>
          fq"$t <- $a.ids"
        }
        val instance = ass.foldLeft(q"$basic": Tree) {
          case (sel, (n, t)) =>
            q"($n := $t.toList) ->: $sel"
        }
        q"for(..$assGen) yield $instance"
      }
      List(
        q"def $basic = this.asJson",
        q"def json($iec) = $ctor"
      )
    }

    def mapperCodec = {
      val names = mapperFieldStrings
      val codec = TermName(s"casecodec${names.length}")
      q"""
      implicit def codecJson =
        $codec($mapperTerm.apply, $mapperTerm.unapply)(..$names)
      """
    }

    def backendMapper = {
      val fields = attrsWithId map { f => q"${f.colName} = ${f.term}" }
      val cls = q"""
      case class $mapperType(..$mapperParams)
      extends tryp.slick.sync.BackendMapper[$tpe]
      {
        def model = $term(..$fields)
      }
      """
      val comp = q"""
      object $mapperTerm
      {
        $mapperCodec
      }
      """
      List(cls, comp)
    }

    def pendingActions = {
      q"""
      val pendingActionsSchema = schema.pending
      """
    }

    lazy val mapper = s"${name}Mapper"

    lazy val mapperType = TypeName(mapper)

    lazy val mapperTerm = TermName(mapper)

    lazy val mapperParams = mapperFields map { _.mapperParam }

    override def modifiedHook = q"""
    def modified(id: $idType) = {
      $term.recordUpdate(id)
    }
    """

    def syncMetadata = {
      q"tryp.slick.sync.SyncTableMetadata($tableName, $fqQueryTerm)"
    }
  }

  override def modelOps(cls: ModelSpec) = new SyncModelOps(cls)

  object SyncEnumProcessor
  extends BasicEnumProcessor
  {
    override def apply(enum: EnumSpec) = super.apply(enum) :+ jsonEncode(enum)

    override def implicitTerms(enum: EnumSpec) =
      jsonTerm(enum) :: super.implicitTerms(enum)

    def jsonTerm(enum: EnumSpec) = TermName(s"enum${enum.name}Codec")

    def jsonEncode(enum: EnumSpec) = {
      val term = enum.term
      q"""
      implicit val ${jsonTerm(enum)}: CodecJson[$term.Value] =
        enumCodecJson($term)
      """
    }
  }

  override val enumProcessor = SyncEnumProcessor

  override def imports = super.imports ++ Seq(
    q"import io.circe._",
    q"import pending.Exports._",
    q"import tryp.slick.Codec._"
  )

  override def extra(models: ModelTree) = {
    List(syncMetadata(models), pendingActionsSchema)
  }

  def pendingActionsSchema = {
    q"""
    val pendingActionsSchema = new tryp.slick.sync.schema.PendingActions.Slick
    """
  }

  def syncMetadata(models: ModelTree) = {
    val data = models.topLevel map { m => q"${m.queryPath} -> ${m.syncMetadata}" }
    q"val syncMetadata = SyncSchemaMetadata(Map(..$data))"
  }

  override def schemaBase = tq"tryp.slick.sync.SyncSchema".tp

  override def schemaProfile = tq"tryp.slick.sync.SyncProfile".tp

  override def schemaBases = tq"tryp.JodaExt".tp :: super.schemaBases
}
