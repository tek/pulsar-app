package tryp
package app

// Creates a ModelsSpec from an annotated module and folds it over a sequence
// of transformer functors.
// A ModelsSpec holds a set of case classes that represent data models.
// Transformers should create framework specific bindings for these models,
// like database table definitions or javascript models.
trait ModelsBase
extends ModelInterfaces
{
  import c.universe._

  def modelsSpec(body: BodyData) = modelsParser(body)

  def transformModels(annottees: Annottees) = {
    val models = (modelsSpec(annottees.comp.body) /: modelsPipeline) {
      case (m, trans) =>
        trans(m)
    }
    Log.t("done transforming models")
    (Annottees.comp ^|-> ModuleData.body).set(models.body)(annottees)
  }

  def transformers = transformModels _ :: Nil

  def modelsPipeline =
    preModelsTransformers ++ modelsTransformers ++ postModelsTransformers

  def preModelsTransformers: List[ModelsTransformer] =
    EnumReifierTransformer :: Nil

  def modelsTransformers: List[ModelsTransformer]

  def postModelsTransformers = Nil

  trait ModelsTransformer
  extends Transformer
  with ModelsTransformerBase
  {
    def apply(models: ModelsSpec): ModelsSpec
    def apply(annottees: Annottees) = annottees
  }

  object EnumReifierTransformer
  extends ModelsTransformer
  {
    def apply(spec: ModelsSpec) = {
      Log.d("reifying enums")
      val modules = spec.enums map(_.module)
      val container = q"object Enums { ..$modules }"
      spec.mergeTree(container)
    }
  }
}
