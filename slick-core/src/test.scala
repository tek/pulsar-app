package tryp.slick

import reflect.macros.whitebox.Context

class MetaTestSchema
extends annotation.StaticAnnotation
{
  def macroTransform(annottees: Any*): Any = macro SlickMacrosTest.process
}

class SlickMacrosTest(val c: Context)
extends SlickBase
{
  import c.universe._

  object SchemaTestTransformer
  extends SlickBaseTransformer
  {
    override implicit def modelOps(cls: ModelSpec) = new ModelOps(cls)

    override def apply(spec: ModelsSpec) = {
      val modelComps = spec.models
        .subForest
        .map { a => a.rootLabel.term }
        .toList
      val alphaType =
        AttrSpec.actualType(spec.models.subForest.head.rootLabel.tpe)
      val gammaModel = spec.models.subForest.last.rootLabel
      val assocType = AttrSpec.actualType(gammaModel.params.head.tpt)
      val optAttr = gammaModel.params(1)
      val optType = AttrSpec.actualType(optAttr.tpt)
      val alphaIsModel = spec.isModel(optType)
      val alpFk = gammaModel.foreignKeys.last
      val betaAssoc = gammaModel.assocTables.head
      val betaAssocKey1 = betaAssoc.params(0)
      val betaAssocKey2 = betaAssoc.params(1)
      val betaAttr = gammaModel.assocs.head
      val original = q"""
      object Original
      {
        ..${spec.models}
        ..${spec.enums}
      }
      """
      val test = q"""
      trait MetaTest
      extends org.specs2.mutable.Specification
      {
        import Original._

        "info" >> {
          $modelComps must_== List(Alpha, Beta, Gamma)
        }

        "actualType" >> {
          classOf[$alphaType] must_== classOf[Alpha]
          classOf[$assocType] must_== classOf[Beta]
          classOf[$optType] must_== classOf[Alpha]
          ${optAttr.option} must beTrue
          $alphaIsModel must beTrue
          ${betaAssocKey1.name.toString} must_== "gamma"
          ${betaAssocKey2.name.toString} must_== "bet"
          ${betaAttr.query} must_== Beta
          ${alpFk.colName.toString} must_== "alpId"
          ${alpFk.sqlFk} must_== "alp"
          ${alpFk.sqlColId} must_== "alp_id"
        }
      }
      """
      spec.mergeTrees(original, test)
    }
  }

  def modelsTransformers = SchemaTestTransformer :: Nil
}

trait SlickTestDb
{
  implicit val dbInfo = TestFileDbInfo(s"slick_test_${this.className}")
  val profile = dbInfo.profile
  val api = profile.api
}
