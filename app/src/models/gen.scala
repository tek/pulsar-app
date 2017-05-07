package tryp.app

import reflect.macros.blackbox.Context
import annotation.StaticAnnotation

import scalaz._, Scalaz._

import tryp.macros._

trait ModelsGenerator
extends ModelsBase
{
  import c.universe._

  object ModelsGeneratorTransformer
  extends ModelsTransformer
  {
    def apply(custom: ModelsSpec) = {
      val q"object $name extends ..$bases { ..$body }" = modelDesc
      val base = BodyData.parse(body)
      val desc = modelsSpec(base)
      Log.d(s"merging custom overrides ($custom) with model desc ($desc)")
      desc ⊹ custom
    }
  }

  object ModelParamsTransformer
  extends ModelsTransformer
  {
    def model(spec: ModelSpec, sub: Seq[Tree]): Seq[Tree] = {
      Stream {
        q"""
        object ${spec.term}
        {
          ..$sub
        }
        """
      }
    }

    def modelParams(spec: ModelsSpec) = {
      val models = spec.liftModels(model)
      q"""
      object ModelParams
      {
        ..$models
      }
      """
    }

    def apply(custom: ModelsSpec) = {
      val mod = ModuleData.parse(modelParams(custom))
      (ModelsSpec.body ^|-> BodyData.modules)
        .append(List(mod))(custom)
    }
  }

  def modelDesc: ModuleDef

  override def preModelsTransformers =
    ModelsGeneratorTransformer :: super.preModelsTransformers

  // override def postModelsTransformers =
  //   ModelParamsTransformer :: super.postModelsTransformers
}

class ModelsGenGen
extends StaticAnnotation
{
  def macroTransform(annottees: Any*) = macro ModelsGenGenAnnotation.process
}

class ModelsGenGenAnnotation(val c: Context)
extends Annotation
{
  import c.universe._

  object GeneratorGenerator
  extends Transformer
  {
    override def apply(annottees: Annottees) = {
      val mod = annottees.cls
      val tree = q"..${annottees.cls.reify}"
      val cls = annottees.cls
      val gen = q"""
      trait ${cls.tpe}
      extends tryp.app.ModelsGenerator
      {
        import c.universe._

        def modelDesc = reify { $tree }.tree match {
          case Block((c @ ClassDef(_, _, _)) :: Nil, expr) => c
          case a =>
            c.abort(c.enclosingPosition, "invalid model generator data")
        }
      }
      """
      Annottees.cls.set(ClassData.parse(gen))(annottees)
    }
  }

  def transformers = GeneratorGenerator :: Nil
}

class ModelsImpl
extends StaticAnnotation
{
  def macroTransform(annottees: Any*) = macro ModelsTranformerGen.process
}

class ModelsTranformerGen(val c: Context)
extends Annotation
{
  import c.universe._

  override def setPositions = false

  val createModelsTransformer = (annottees: Annottees) => {
    val transType = annottees.tpe.suffix("Transformer")
    val transTerm = transType.toTermName
    val transBase = paramsS.headOption
      .map(TypeName(_))
      .getOrElse(TypeName("ModelsTransformer"))
    val bases = annottees.cls.bases :+ tq"ModelsBase"
    val trans = q"""
    trait ${annottees.tpe}
    extends ..$bases
    {
      import c.universe._

      trait $transType
      extends $transBase
      {
        ..${annottees.body}
      }

      object $transTerm
      extends $transType
    }
    """
    Annottees.cls.set(ClassData.parse(trans))(annottees)
  }

  def transformers = createModelsTransformer :: Nil
}
