package tryp
package slick

import scala.reflect.macros.whitebox.Context

import scalaz._, Scalaz._
import syntax.std.boolean._
import syntax.std.option._

import tryp.macros.Annotation
import tryp.app._

@ModelsImpl
trait SlickBase
{
  val reservedNames = List("id", "created", "updated")

  def sqlColName(name: String): String = {
    name.toCharArray().zipWithIndex map {
      case (ch, i) if Character.isUpperCase(ch) && i > 0 =>
        "_" + Character.toLowerCase(ch)
      case (ch, _) => Character.toLowerCase(ch)
    } mkString
  }

  override def resourceSpecOps(res: ResourceSpec) = {
    res match {
      case m: ModelSpec => modelOps(m)
      case a: AssocSpec => assocOps(a)
      case _ => abort(s"invalid resource spec: $res")
    }
  }

  implicit def tableOps(res: ResourceSpec): TableOps = {
    resourceSpecOps(res) match {
      case t: TableOps => t
      case _ => abort(s"invalid input for TableOps conversion: $res")
    }
  }

  implicit def modelOps(m: ModelSpec) = new ModelOps(m)
  def assocOps(a: AssocSpec) = new AssocOps(a)

  class BasicEnumProcessor
  {
    def apply(enum: EnumSpec): List[Tree] = List(
      mapper(enum)
    )

    def mapperTerm(enum: EnumSpec) = enum.term.suffix("TypedType")

    def mapper(enum: EnumSpec): Tree = {
      q"""
      implicit val ${mapperTerm(enum)} =
        tryp.slick.meta.TypedType.enum(${enum.term})
      """
    }

    def implicitTerms(enum: EnumSpec) = mapperTerm(enum) :: Nil

    def implicits(enum: EnumSpec) = {
      implicitTerms(enum) map { term => q"implicit val $term = schema.$term" }
    }
  }

  case class AssocTableTargetAttrSpec(original: ModelListSpec)
  extends SingleModelSpec
  {
    val tpt = original.actualType.nameTree

    val field = FieldData.parse(q"val ${original.singularName}: $tpt")

    val customDefault = q""

    def model = original.model

    def targetType = original.actualType

    def query = original.fqQueryTerm
  }

  trait SlickOps
  extends Ops
  {
    trait SlickAttrOpsBase
    extends AttrOpsBase
    {
      lazy val option = tpt match {
        case tq"Option[..$_]" => true
        case _ => false
      }

      lazy val ids = term.suffix("Ids").d

      lazy val attrQuery = term.suffix("Query").d

      def crud(s: String) = term.u.prefix(s)

      lazy val add = crud("add").singular

      lazy val remove = crud("remove")

      lazy val delete = crud("delete")

      lazy val replace = crud("replace")

      def sqlColId = name.snakeCase

      lazy val assocQueryColId = term.ds.columnId

      lazy val singularName = term.singular

      def defaultValDef = q"val $term: $tpt = $default"

      lazy val paramName = term

      def column = q"def $colName = column[$colType]($sqlColId, ..$columnFlags)"

      lazy val colName = term

      lazy val colId = term.columnId

      def colType = tpt

      def columnFlags: List[Select] = Nil

      def singularTerm = term.ds

      def tilde: Tree = q"$colName"

      def mapperParam = defaultValDef
    }

    case class SlickAttrOps(attr: AttrSpec)
    extends SlickAttrOpsBase
    with SimpleAttrOpsBase

    trait SlickModelAttrOps
    extends SlickAttrOpsBase
    with ModelAttrOpsBase
    {
      def fqQueryType = attr.actualType

      def fqQueryTerm = attr.actualTerm

      def fqQueryTable = attr.actualType.suffix("Table")

      def tableType = fqQueryTable

      def query = fqQueryTerm

      def refPath = attr.actualType match {
        case \/-(select) => s"${select.qualifier.toString.toLowerCase}_$name"
        case -\/(tpe) => name
      }

      def noRecAdapter = attr.term.u.prefix("noRecord")
    }

    trait SlickEmbeddedOps
    extends SlickModelAttrOps
    with EmbeddedOpsBase
    {
      def attr: EmbeddedSpec

      override def fqQueryTerm = attrPathTerm

      def queryBase = attr.actualType.suffix("Base")

      def tableName = attrPathTerm.stringify.toLowerCase

      def metadata = q"EmbeddedAttrMetadata($tableName, $fqQueryTerm)"
    }

    trait SlickSingleModelOps
    extends SlickModelAttrOps
    {
      def sqlFk = name.snakeCase

      def fkDef = q"def $term = foreignKey($sqlFk, $colId, $query)(_.id)"
    }

    case class SlickRefSingleOps(attr: RefSingleSpec)
    extends SlickSingleModelOps
    with RefSingleOpsBase
    {
      import ScalazGlobals._

      override def colType = option ? tq"Option[$keyType]" | keyType

      def keyType = idType

      override lazy val colName = colId

      override def sqlColId = name.colIdName.snakeCase

      override def ctorTerm = colId

      override def ctorTpt = colType

      override lazy val paramName = colName

      override def mapperParam = q"val $term: $keyType"
    }

    trait SlickModelListOps
    extends SlickModelAttrOps
    with ModelListOpsBase
    {
      def keyType = idType

      override def column = sys.error("RefListSpec can't become column")

      override def mapperParam = q"val $term: List[$keyType]"
    }

    case class SlickRefListOps(attr: RefListSpec)
    extends SlickModelListOps
    with RefListOpsBase

    case class SlickEmbeddedSingleOps(attr: EmbeddedSingleSpec)
    extends SlickSingleModelOps
    with EmbeddedSingleOpsBase
    with SlickEmbeddedOps

    case class SlickEmbeddedListOps(attr: EmbeddedListSpec)
    extends SlickModelListOps
    with EmbeddedListOpsBase
    with SlickEmbeddedOps

    case class SlickIdOps(attr: IdSpec)
    extends SlickAttrOpsBase
    with IdOpsBase
    {
      override def columnFlags = List(q"O.PrimaryKey")
    }

    override def attrBase(a: AttrSpecBase) =
      new SlickAttrOpsBase { def attr = a }

    override def attr(a: AttrSpec) = SlickAttrOps(a)

    override def id(a: IdSpec) = SlickIdOps(a)

    override def refSingle(a: RefSingleSpec) = SlickRefSingleOps(a)

    override def refList(a: RefListSpec) = SlickRefListOps(a)

    override def embeddedSingle(a: EmbeddedSingleSpec) =
      SlickEmbeddedSingleOps(a)

    override def embeddedList(a: EmbeddedListSpec) = SlickEmbeddedListOps(a)
  }

  object SlickOps
  extends SlickOps

  override val ops = SlickOps

  implicit def slickAttrOps(base: AttrSpecBase) =
    opsToOpt[SlickOps.SlickAttrOpsBase](base) match {
      case Some(a) => a
      case None => new SlickOps.SlickAttrOpsBase { def attr = base }
    }

  implicit def slickModelAttrOps(attr: ModelAttrSpec) =
    opsTo[SlickOps.SlickModelAttrOps](attr)

  implicit def slickSingleModelOps(attr: SingleModelSpec) =
    opsTo[SlickOps.SlickSingleModelOps](attr)

  implicit def slickModelListOps(attr: ModelListSpec) =
    opsTo[SlickOps.SlickModelListOps](attr)

  implicit def slickEmbeddedOps(attr: EmbeddedSpec) =
    opsTo[SlickOps.SlickEmbeddedOps](attr)

  case class AssocSpec(tpe: TypeName, from: RefSingleSpec,
    to: AssocTableTargetAttrSpec, basic: TreeLoc[ModelTreeNode])
  extends ResourceSpec
  {
    def params = List(from, to)
    def bases = Nil
  }

  abstract class TableOps(val res: ResourceSpec)
  extends ResourceOpsBase
  {
    def tableName = name.toLowerCase

    def queryPath = name.toLowerCase

    lazy val assocQuerys = assocs map(assocName)

    lazy val names = name :: assocQuerys

    lazy val queries = names map(_.dp)

    lazy val tableType = tpe.suffix("Table")

    lazy val query = term

    def fqQueryTerm = res.fqTerm(query)

    def fqEmbeddedQueryTerm(attr: AttrSpecBase) =
      q"${fqQueryTerm}.${attr.term.u}"

    lazy val term = tpe.toTermName

    def assocQueryTerm(other: AttrSpecBase) = TermName(assocName(other))

    def assocModel(other: AttrSpecBase) = TermName(assocName(other))

    def assocName(other: AttrSpecBase) = assocTableName(other)

    def assocTableName(to: AttrSpecBase) =
      s"${name}2${to.term.u}"

    def queryBaseType = tpe.suffix("Base")

    lazy val foreignKeys = res.fieldType[SingleModelSpec]

    lazy val assocs = res.fieldType[ModelListSpec]

    lazy val embeddedAssocs = res.fieldType[EmbeddedListSpec]

    lazy val refAssocs = res.fieldType[RefListSpec]

    lazy val attrs = res.fieldType[SimpleAttrSpec]

    // the fields owned by the model case class
    // excludes associations
    override def modelFields = dataFields

    def dataFields = attrs ++ foreignKeys

    lazy val colId = term.columnId

    def tildeFields = modelFields map(_.tilde)

    val sqlTableId = name.dp.toLowerCase

    def columns = modelFields map(_.column)

    def modelBases: List[Tree] = Nil

    def queryType: Tree = tq"CrudQuery[$tpe, $tableType]"

    def queryExtra: List[Tree] = List(extractor)

    def extractor = {
      val fields = extractorFields map { f => q"m.${f.term}" }
      q"""
      object e
      {
        def unapply(m: $tpe) = Some((..$fields))
      }
      """
    }

    def extractorFields: Attrs = attrs

    def times = {
      q"""
      def * = (..$tildeFields).shaped <> (
        ($term.apply _).tupled, $term.unapply
      )
      """
    }

    def modelExtra: List[Tree] = Nil

    def metadata = q"TableMetadata($tableName, $fqQueryTerm)"
  }

  class ModelOps(m: ModelSpec)
  extends TableOps(m)
  {
    lazy val assocTableSource =
      DefaultModelsParser.attrFromData(term.d, -\/(tpe), Some(m.basic))

    def assocTables = {
      assocs.map { ass =>
        val assType = TypeName(assocName(ass))
        val fks = List(assocTableSource, AssocTableTargetAttrSpec(ass))
        val (from, to) = fks match {
          case List(f: RefSingleSpec, t: AssocTableTargetAttrSpec) => f -> t
          case _ =>
            abort(s"assoc table fks weren't parsed as SingleModelSpec: " +
              s"$fks ($assType)")
        }
        AssocSpec(assType, from, to, m.basic)
      }
    }

    def embeddedQuery(attr: TermName) = \/-(q"$term.${attr.u}")

    lazy val idColumn = IdSpec()

    override def modelFields = nonDateFields ++ dateColumns

    def nonDateFields = dataFields ++ extraColumns

    override def extractorFields = super.extractorFields :+ idColumn

    override def queryExtra = super.queryExtra :+ modifiedHook :+ withDate

    override def modelExtra = m.body.reify

    def modifiedHook = q"def modified(id: $idType) = nopDbio"

    def withDate = {
      val fields = nonDateFields map { a => q"obj.${a.paramName}" }
      q"""
      def withDate(obj: $tpe, u: DateTime) = {
        $term(..$fields, created = obj.created, updated = u)
      }
      """
    }

    def extraColumns: Attrs = List(idColumn)

    def dateColumns = List(DateSpec("created"), DateSpec("updated"))

    def isNamed = attrs exists(_.name == "name")

    import ScalazGlobals._

    def modelBase = isNamed ? tq"tryp.slick.NamedModel" | tq"tryp.slick.Model"

    override def modelBases =
      modelBase ::
      bases

    def compact = {
      q"""
      def compactString = {
        val params = List(..${attrs map(_.term)}).mkString("(", ", ", ")")
        $name + params
      }
      """
    }

    def verbose = {
      val fks = foreignKeys map(k => q"${k.term}.!!.show")
      val ass = assocs map {
        a => q"""${a.term}.all.!! map(_.show) mkString("{", ",", "}")"""
      }
      q"""
      def verbose(implicit ec: scala.concurrent.ExecutionContext,
        dbi: tryp.slick.DbInfo) = {
        import scalaz._, Scalaz._
        val params = List(..${dataFields map(_.term)}, ..$fks, ..$ass)
          .mkString("(", ", ", ")")
        $name + params
      }
      """
    }

    def extra: List[Tree] = Nil

    def genParamsType = {
      dataFields
        .foldRight(tq"HNil": Tree) { (f, agg) => tq"${f.colType} :: $agg" }
    }
  }

  // FIXME prefix tableName with source attr model path parents
  class AssocOps(m: AssocSpec)
  extends TableOps(m)
  {
    override def tableName = name.toLowerCase
  }

  def apply(spec: ModelsSpec) = spec
}
