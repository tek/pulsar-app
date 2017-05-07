package tryp.slick

import scalaz._, Scalaz._

import tryp.app._

// TODO only pass 'SlickBase', inherit automatically
@ModelsImpl(SlickBaseTransformer)
trait Slick
extends SlickBase
{
  class SlickModelInterface(m: ModelSpec)
  extends ModelInterface(m)
  {
    def create = {
      ModelImpl.parse(m, model, comp, extra: _*)
    }

    def extra = {
      table :: queryBase :: assocTables ::: m.extra
    }

    def model = {
      val fks = m.foreignKeys.map { field =>
        List(
          q"""
          def ${field.attrQuery}($iec) =
            ${field.query}.filter { _.id === ${field.colId} }
          """,
          q"""
          def ${field.term}($iec) = ${field.attrQuery}.one
          """
        )
      } flatten
      val assocs = m.assocs.map { f =>
        val attr = f.term
        val adapterAttr = attr.suffix("Adapter")
        val assocQuery = q"${m.assocQueryTerm(f)}"
        val otherQuery = f.fqQueryTerm
        val attrType = f.actualType
        val attrTableType = f.tableType
        val adapterType = tq"AssocAdapter[${m.tpe}, $attrType, $attrTableType]"
        List(
          q"""
          def $adapterAttr(record: Boolean) =
            new $adapterType(id, $assocQuery, $otherQuery, record)
          """,
          q"""
          def $attr = $adapterAttr(true)
          """,
          q"""
          def ${f.noRecAdapter} = $adapterAttr(false)
          """
        )
      }.flatten
      val adapterByName = {
        val adapterCases = m.assocs.map { f =>
          val adapterAttr = f.term.suffix("Adapter")
          cq"${f.name.toLowerCase} => Some($adapterAttr(record))"
        }
        q"""
        def adapter(name: String, record: Boolean = true) = {
          name match {
            case ..$adapterCases
            case _ => {
              tryp.Log.e(
                s"Requested adapter for nonexisting association '$$name'")
              None
            }
          }
        }
        """
      }
      q"""
      case class ${m.tpe}(..${m.ctorParams})
      extends ..${m.modelBases}
      {
        def meta = ${m.term}
        def path = meta.path
        ..$fks
        ..$assocs
        $adapterByName
        ${m.compact}
        ${m.verbose}
        ..${m.modelExtra}
      }
      """
    }

    def table = {
      val foreignKeys = m.foreignKeys.map { _.fkDef }
      val assocs = m.assocs.map { f =>
        val assocQuery = q"${m.assocQueryTerm(f)}"
        val otherQuery = q"${f.query}"
        val model = m.assocModel(f)
        val myId = m.colId
        val otherId = f.colId
        Seq(
          q"""
          def ${f.term} = for {
            x <- $assocQuery
            if x.fromId === id
            y <- $otherQuery
            if x.toId === y.id
          } yield(y)
          """
        )
      }.flatten
      q"""
      class ${m.tableType}(tag: Tag, tableName: String)
      extends profile.Table[${m.tpe}](tag, tableName)
      with tryp.slick.ModelTableI[${m.tpe}]
      {
        ..${m.columns}
        ..$assocs
        ..$foreignKeys
        ${m.times}
      }
      """
    }

    def queryBase = {
      // the broken map chain in `gen` is due to a handling problem in the
      // position attachment process
      q"""
      abstract class ${m.queryBaseType}(name: String)
      extends ${m.queryType}(tag => new ${m.tableType}(tag, name))
      {
        def create(..${m.ctorParams})($iec) =
          this.insert(${m.term}(..${m.ctorArgs}))

        def gen(data: ${m.genParamsType}*)($iec) = {
          val a = data
            .map(_ ::: MNil)
          a.map(generic.from)
            .map(this.insert)
            .toDbioSeq
        }

        ..${m.queryExtra}

        def generic = Generic[${m.tpe}]
      }
      """
    }

    def comp = {
      val embed = m.embeddedAssocs map { ass =>
        q"""
        object ${ass.term.u}
        extends ${ass.queryBase}(${ass.tableName})
        {
          def name = ${ass.term.toString.toLowerCase}
          def path = ${ass.tableName}
        }
        """
      }
      q"""
      implicit object ${m.term}
      extends ${m.queryBaseType}(${m.tableName})
      {
        def name = ${m.queryPath}
        def path = ${m.queryPath}
        ..$embed
      }
      """
    }

    def assocTables: List[Tree] = {
      m.assocTables
        .map(assocQuery)
    }

    def assocQuery(ass: AssocSpec) = {
      val from = ass.from.tpe
      val to = ass.to.targetType
      val toQuery = ass.to.query
      q"""
      implicit object ${ass.term}
      extends AssocTableQuery[$from, $to](
        ${ass.tableName}, ${toQuery})
      """
    }
  }

  def modelInterface(cls: ModelSpec) = new SlickModelInterface(cls)

  def schemaMetadata(models: ModelTree) = {
    def single(spec: ResourceSpec) = {
      q"${spec.tableName} -> ${spec.metadata}"
    }
    def embedded(spec: EmbeddedSpec) = {
      q"${spec.tableName} -> ${spec.metadata}"
    }
    val data = models.liftSub { (model, sub) =>
      single(model) :: sub.toList ::: model.assocTables.map(single) :::
        model.embeddedAssocs.map(embedded)
    }
    q"val metadata = SchemaMetadata(Map(..$data))"
  }

  def metadata(models: ModelTree) = schemaMetadata(models) :: Nil

  def schemaBase = tq"tryp.slick.Schema".tp

  def schemaProfile = tq"tryp.slick.Profile".tp

  def schemaBases = List(schemaProfile)

  def extra(classes: ModelTree): List[Tree] = Nil

  def extraPre(classes: ModelTree): List[Tree] = Nil

  def imports = {
    List(
      q"import tryp.slick.meta.SchemaImports._",
      q"import shapeless._",
      q"import profile.api._",
      q"import Enums._"
    )
  }

  def exports(models: ModelTree, enums: List[EnumSpec]) = {
    def term(n: TermName) = q"val $n = schema.$n"
    val resources = {
      models.topLevel
        .map(_.term)
        .map { n =>
          List(
            q"type ${n.toTypeName} = schema.${n.toTypeName}",
            term(n)
          )
        }
        .flatten
    }
    val enum = enums map(_.term) map(a => q"val $a = Enums.$a")
    val enumImplicits = enums.map(enumProcessor.implicits).flatten
    List(
      q"""
      trait Exports
      {
        ..$resources
        ..$enum
      }
      """,
      q"""
      object Exports
      extends Exports
      """,
      q"""
      trait Implicits
      extends Exports
      {
        ..$enumImplicits
      }
      """,
      q"""
      object Implicits
      extends Implicits
      """
    )
  }

  val enumProcessor = new BasicEnumProcessor

  def database(models: ModelTree) = models map(modelInterface) map(_.create)

  def handle(models: ModelTree, enums: List[EnumSpec]) = {
    val enum = enums.map(enumProcessor.apply).flatten
    q"""
    class Slick()(implicit prof: _root_.slick.jdbc.JdbcProfile)
    extends $schemaBase with ..$schemaBases
    { schema =>
      ..$imports
      ..${extraPre(models)}
      ..$enum
      ..${database(models)}
      ..${metadata(models)}
      ..${extra(models)}
      ..${exports(models, enums)}
    }
    """
  }

  override def apply(spec: ModelsSpec) = {
    val models = spec.modelsFor("droid")
    spec.mergeTree(handle(models, spec.enums))
  }
}
