package tryp.spray

import reflect.macros._

import scalaz._, Scalaz._

import tryp.app._

@ModelsImpl
trait MongoSpray
{
  object MongoSprayOps
  extends Ops
  {
    trait SprayAttrOpsBase
    extends AttrOpsBase
    {
      def creatorParam = {
        val term = attr.term
        if (isOptional) q"$term = $term.getOrElse($default)"
        else q"$term = $term"
      }

      def updaterParam = {
        val tname = attr.term
        if (attr.option) q"$tname = $tname.orElse(o.$tname)"
        else q"$tname = $tname getOrElse(o.$tname)"
      }

      def toBson(n: TermPath) = q"$n.toBSON"

      def bsonUpdateParam = {
        val tname = attr.term
        if (attr.option) q"$tname.map(a => $name -> ${toBson(tname.left)})"
        else q"$tname.map(a => $name -> ${toBson(TermName("a").left)})"
      }

      def msType = tpt

      def msValDef = q"$mods val $term: $msType = $default"

      def optionValDefNone = {
        if (attr.option) q"$mods val $term: $msType = None"
        else q"$mods val $term: Option[$msType] = None"
      }

      def postValDef = {
        if (isOptional) q"$mods val $term: Option[$msType] = None"
        else q"$mods val $term: $msType = $postDefault"
      }

      def postDefault = default

      def putValDef = optionValDefNone

      def path = name.toLowerCase

      def dao = attr.actualTerm

      def bsonGet = q"bson.getAsTry[$msType](transformForBSON($name))"

      def readerOptional = isOptional

      def bsonReaderParam = {
        val alt =
          if (readerOptional) default
        else
          q"""
          sys.error(
            s"Couldn't parse bson value for attr '" + $name + "' in object " +
            s"$${bson.show}: $$e"
          )
          """
        q"""
        $term = $bsonGet match {
          case util.Success(a) => a
          case util.Failure(e) => $alt
        }
        """
      }

      def bsonWriterParam = {
        val to = toBson(q"o.$term".right)
        q"transformForBSON($name) -> $to"
      }
    }

    case class SprayEmbeddedSingleOps(attr: EmbeddedSingleSpec)
    extends EmbeddedSingleOpsBase
    with SprayAttrOpsBase
    {
      override def toBson(n: TermPath) = q"$n.toBSONDoc"

      override def bsonGet = {
        q"bson.getAsTry[$msType](transformForBSON($name))($dao.bsonDocMapper)"
      }
    }

    case class SprayEmbeddedListOps(attr: EmbeddedListSpec)
    extends EmbeddedListOpsBase
    with SprayAttrOpsBase
    {
      override def isOptional = true

      override def msType = monadWithModelType
    }

    case class SprayRefSingleOps(attr: RefSingleSpec)
    extends RefSingleOpsBase
    with SprayAttrOpsBase
    {
      override def toBson(n: TermPath) = super.toBson(n)

      override def msType = idType
    }

    case class SprayRefListOps(attr: RefListSpec)
    extends RefListOpsBase
    with SprayAttrOpsBase
    {
      override def isOptional = true

      override def msType = monadWithIdType
    }

    case class SprayIdOps(attr: IdSpec)
    extends SprayAttrOpsBase
    with IdOpsBase
    {
      override def postValDef = field.asOption

      def field = attr.field.withAnnotation("opt")

      override def isOptional = true

      override def readerOptional = false
    }

    override def attrBase(a: AttrSpecBase) =
      new SprayAttrOpsBase { def attr = a }

    override def id(a: IdSpec) = SprayIdOps(a)

    override def refSingle(a: RefSingleSpec) = SprayRefSingleOps(a)

    override def refList(a: RefListSpec) = SprayRefListOps(a)

    override def embeddedSingle(a: EmbeddedSingleSpec) =
      SprayEmbeddedSingleOps(a)

    override def embeddedList(a: EmbeddedListSpec) = SprayEmbeddedListOps(a)
  }

  override val ops = MongoSprayOps

  implicit def sprayAttrOps(base: AttrSpecBase) =
    opsToOpt[MongoSprayOps.SprayAttrOpsBase](base) match {
      case Some(a) => a
      case None => new MongoSprayOps.SprayAttrOpsBase { def attr = base }
    }

  implicit class MsModelOps(val res: ModelSpec)
  extends ResourceOpsBase
  {
    def modelTerm = res.term

    def modelType = res.tpe

    def daoType = modelType.suffix("Dao")

    val idField = IdSpec()

    val attrsWithoutId = params

    override def modelFields = params :+ idField

    def hasRef = !res.refParams.isEmpty

    def dbName = res.ident.toLowerCase

    def codecs(term: TermPath) = {
      val codec = term.suffix("Codec")
      List(tq"$codec.PostCodec", tq"$codec.PutCodec")
    }

    def embeddedRouterCases = res.embeddedLists map { field =>
      val tpe = field.actualType
      val term = field.actualTerm
      cq"""
      ${field.path} =>
        new tryp.spray.CrudRouter[${field.actualType}, ..${codecs(term)}](
          params => tryp.spray.EmbeddedAttrAdapter[${field.actualType}](
            ${field.name}, params, path)
        ).routes
      """
    }

    def refRouterCases = res.refLists map { field =>
      val term = field.actualTerm
      cq"""
      ${field.path} =>
        new tryp.spray.CrudRouter[${field.actualType}, ..${codecs(term)}](
          params => tryp.spray.RefAttrAdapter[${field.actualType}](
            ${field.name}, ${field.term}, params, path)
        ).routes
      """
    }

    def routerCases = embeddedRouterCases ++ refRouterCases

    lazy val attrRouter = {
      q"""
      def routes(attr: String, path: tryp.mongo.DefinedPath)
      (implicit ec: $EC, mongo: MongoDb) = {
        attr match {
          case ..$routerCases
          case _ => throw tryp.spray.AttrTypeError(id, attr)
        }
      }
      """
    }

    lazy val restRouter = {
      val term = modelTerm
      List(
        q"""
        def routes($iec) = {
          implicit val d = this
          new tryp.spray.CrudRouter[$tpe, ..${codecs(term.left)}](
            params => tryp.spray.FlatAdapter[$tpe](params)
          ).routes
        }
        """
        ,
        q"""
        def root($iec) = {
          Directives.pathPrefix(name)(routes)
        }
        """
      )
    }

    val postFields = modelFields map(_.postValDef)

    val putFields = attrsWithoutId map(_.putValDef)

    def fieldStrings(f: List[Tree]) = f collect {
      case q"$mods val $tname: $tpt = $expr" =>
        tname.toString
    }

    def creatorParams = modelFields map(_.creatorParam)

    def updaterParams = attrsWithoutId map(_.updaterParam)

    def bsonUpdateParams = attrsWithoutId map(_.bsonUpdateParam)

    val codecTerm = modelTerm.suffix("Codec")

    val clsParams = modelFields map(_.msValDef)

    def bsonDocMapper = {
      val rParams = modelFields map(_.bsonReaderParam)
      val wParams = modelFields map(_.bsonWriterParam)
      q"""
      implicit val bsonDocMapper: tryp.mongo.BSONDocTypeMapper[$tpe] =
        new tryp.mongo.BSONDocTypeMapper[$tpe] {
          def write(o: $tpe) = BSONDocument(..$wParams)
          def readDoc(bson: BSONDocument) = $modelTerm(..$rParams)
        }
      """
    }

    def daoInstTerm = TermName(res.modelPath.map(_.ident).mkString)

    def daoInstBase = res.basic.modelTypePath.withName(res.daoType)

    def daoInstPath = res.basic.modelTermPath
  }

  override def resourceSpecOps(cls: ResourceSpec) = {
    cls match {
      case m: ModelSpec => new MsModelOps(m)
      case _ => super.resourceSpecOps(cls)
    }
  }

  class SprayModelInterface(m: ModelSpec)
  extends ModelInterface(m)
  {
    def create = ModelImpl.parse(m, model, comp, dao, codec)

    def codec = {
      q"""
      object ${m.codecTerm}
      {
        case class PostCodec(..${m.postFields})
        extends tryp.spray.ModelCreator[${m.tpe}]
        {
          def model = ${m.modelTerm}(..${m.creatorParams})
        }

        case class PutCodec(..${m.putFields})
        extends tryp.spray.ModelUpdater[${m.tpe}]
        {
          def bson = BSONDocument(List(..${m.bsonUpdateParams}) flatten)
          def update(o: ${m.tpe}) =
            ${m.modelTerm}(..${m.updaterParams}, id = o.id)
        }

        trait formats
        {
          val json: RootJsonFormat[${m.tpe}] =
              jsonFormat(${m.modelTerm}.apply,
                ..${m.fieldStrings(m.modelFields map(_.valDef))})
          implicit val postJson: RootJsonFormat[PostCodec] =
              jsonFormat(PostCodec.apply, ..${m.fieldStrings(m.postFields)})
          implicit val putJson: RootJsonFormat[PutCodec] =
              jsonFormat(PutCodec.apply, ..${m.fieldStrings(m.putFields)})
        }

        object formats
        extends formats

        object marshal
        extends tryp.spray.Marshalling[${m.tpe}, PostCodec, PutCodec]
        with formats
        with _root_.spray.httpx.SprayJsonSupport
        with _root_.spray.json.DefaultJsonProtocol
        {
          import _root_.spray.httpx.unmarshalling.Deserializer._
          implicit val `generic marshaller`: Marshaller[${m.tpe}] =
            sprayJsonMarshallerConverter[${m.tpe}](json)
          implicit val `generic post unmarshaller` =
            implicitly[Unmarshaller[PostCodec]]
          val post = fromRequestUnmarshaller[PostCodec]
          implicit val `generic put unmarshaller` =
            sprayJsonUnmarshallerConverter[PutCodec](putJson)
          implicit val `fmum put` =
            fromMessageUnmarshaller(`generic put unmarshaller`)
          val put = fromRequestUnmarshaller(`fmum put`)
          val toResponse = implicitly[ToResponseMarshaller[${m.tpe}]]
        }
        def post = marshal.post
        def put = marshal.put
      }
      """
    }

    def dao = {
      q"""
      abstract class ${m.daoType}
      (implicit val mongoDb: MongoDb)
      extends tryp.mongo.Dao[${m.tpe}]
      {
        val name = ${m.dbName}
        ..${m.restRouter}
        ..$queryCommon
        def bsonDocMapper = ${m.modelTerm}.bsonDocMapper
      }
      """
    }

    def comp = {
      q"""
      object ${m.modelTerm}
      {
        val name = ${m.dbName}
        val codec = ${m.codecTerm}
        implicit def json = codec.formats.json
        implicit def marshal = codec.marshal
        implicit val modelParams = new tryp.spray.ModelParams[${m.tpe}] {
          def embeddedNames = List(..${m.embeddedNames})
        }
        implicit def dao(implicit mongo: MongoDb)
        : tryp.mongo.Dao[${m.tpe}] = {
          ${TermName("daos") :: m.daoInstPath}
        }
        ..${m.bsonDocMapper}
      }
      """
    }

    def model = {
      q"""
      case class ${m.tpe}(..${m.ctorParams})
      extends tryp.spray.Model[${m.tpe}]
      {
        def dao(implicit mongo: MongoDb) = ${TermName("daos") :: m.daoInstPath}
        ..${m.attrRouter}
      }
      """
    }
  }

  def modelInterface(cls: ModelSpec) = new SprayModelInterface(cls)

  val imports = Seq(
    q"import _root_.spray.json._",
    q"import _root_.spray.httpx.unmarshalling._",
    q"import _root_.spray.httpx.marshalling._",
    q"import reactivemongo.bson._",
    q"import tryp.spray.ModelImports._",
    q"type ObjectId = tryp.spray.ModelImports.ObjectId",
    q"val ObjectId = tryp.spray.ModelImports.ObjectId",
    q"import Enums._"
  )

  def sprayEnum(enum: EnumSpec) = {
    val term = enum.term
    List(
      q"""
      implicit val ${term.suffix("EnumJsonFormat")} = jsonEnum($term)
      """,
      q"""
      implicit val ${term.suffix("EnumMapper")} = enumTypeMapper($term)
      """
      )
  }

  def schemaTerm = TermName("schema")

  def schemaSelf = q"val $schemaTerm: AnyRef"

  def daoInst(model: ModelSpec) = {
    q"""
    implicit lazy val ${model.daoInstTerm} =
      new ${schemaTerm :: model.daoInstBase} {}
    """
  }

  def daosImplicit = {
    q"""
    implicit def daos(implicit mongo: MongoDb) = new DB
    """
  }

  def dbInterface(spec: ModelsSpec) = {
    val impMongo = TermName(c.freshName)
    val daos = spec.models map { m =>
      q"""
      object ${m.term}
      extends ${schemaTerm :: m.daoInstBase}()($impMongo)
      """
    }
    List(
      q"""
      class DB(implicit $impMongo: MongoDb)
      {
        ..$daos
      }
      """,
      q"""
      trait DbAccess
      {
        implicit def mongoDb: MongoDb
        lazy val mongo = new DB
      }
      """,
      daosImplicit
    )
  }

  def exports(spec: ModelsSpec) = {
    val types = spec.topLevel flatMap { m =>
      List(
        q"type ${m.tpe} = ${schemaTerm :: m.typePath}",
        q"val ${m.term} = ${schemaTerm :: m.termPath}"
      )
    }
    val daos = spec.topLevel map { m =>
      q"lazy val ${m.term.suffix("Dao")} = db.${m.daoInstTerm}"
    }
    val models = q"""
    trait Exports
    {
      ..$types
      def daos(implicit m: MongoDb) = new DB
    }
    """
    val modelsObj = q"object Exports extends Exports"
    val all = q"""
    trait Full
    extends Exports
    {
      implicit def mongoDb: MongoDb
      lazy val db = daos
      ..$daos
    }
    """
    models :: modelsObj :: all :: dbInterface(spec)
  }

  def implicits = {
    q"""
    trait Implicits
    {
      $daosImplicit
    }
    """
  }

  def apply(spec: ModelsSpec) = {
    val enums = spec.enums.map(sprayEnum).flatten
    val models = spec.models map(modelInterface) map(_.create)
    val xp = exports(spec)
    spec.mergeTree {
      q"""
      trait MongoSpray
      extends tryp.spray.meta.SprayTypes
      { $schemaSelf =>
        ..$imports
        ..$enums
        ..$models
        ..$xp
        ..$implicits
      }
      """
    }
  }
}
