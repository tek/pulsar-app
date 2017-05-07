package tryp.annotation

import reflect.macros.whitebox.Context
import annotation.StaticAnnotation

trait SchemaAnnotationBase
extends tryp.slick.Slick
{
  import c.universe._

  val slickTransformer: SlickTransformer = SlickTransformer

  def modelsTransformers = slickTransformer :: Nil
}

class Slick
extends StaticAnnotation
{
  def macroTransform(annottees: Any*) = macro SchemaAnnotation.process
}

class SchemaAnnotation(val c: Context)
extends SchemaAnnotationBase
