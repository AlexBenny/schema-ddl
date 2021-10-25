package com.snowplowanalytics.iglu.schemaddl.trackers

import com.snowplowanalytics.iglu.schemaddl.jsonschema.Schema
import com.snowplowanalytics.iglu.schemaddl.jsonschema.properties.{CommonProperties, StringProperty}
import com.snowplowanalytics.iglu.schemaddl.jsonschema.properties.CommonProperties.Type
import com.snowplowanalytics.iglu.schemaddl.jsonschema.properties.NumberProperty.{Maximum, Minimum, MultipleOf}


abstract class EnrichedProperty {
  def name: String
  def `type`: Type
  def description: Option[String]
  def optional: Boolean
  def alternativeTypes: Option[Set[Type]]
}

case class StringEnrichedProperty(name: String,
                                  `type`: Type = Type.String,
                                  description: Option[String],
                                  optional: Boolean,
                                  alternativeTypes: Option[Set[Type]] = None,
                                  maxLength: Option[BigInt] = None,
                                  minLength: Option[BigInt] = None,
                                  pattern: Option[String] = None,
                                  format: Option[StringProperty.Format] = None
                                 ) extends EnrichedProperty

case class IntegerEnrichedProperty(name: String,
                                   `type`: Type = Type.Integer,
                                   description: Option[String],
                                   optional: Boolean,
                                   alternativeTypes: Option[Set[Type]] = None,
                                   multipleOf: Option[BigInt] = None,
                                   minimum: Option[BigInt] = None,
                                   maximum: Option[BigInt] = None
                                  ) extends EnrichedProperty

case class NumberEnrichedProperty(name: String,
                                  `type`: Type = Type.Number,
                                  description: Option[String],
                                  optional: Boolean,
                                  alternativeTypes: Option[Set[Type]] = None,
                                  multipleOf: Option[BigDecimal] = None,
                                  minimum: Option[BigDecimal] = None,
                                  maximum: Option[BigDecimal] = None
                                 ) extends EnrichedProperty

case class BooleanEnrichedProperty(name: String,
                                   `type`: Type = Type.Boolean,
                                   description: Option[String],
                                   optional: Boolean,
                                   alternativeTypes: Option[Set[Type]] = None
                                  ) extends EnrichedProperty

// TODO: to develop
case class ArrayEnrichedProperty(name: String,
                                 `type`: Type = Type.Array,
                                 description: Option[String],
                                 optional: Boolean,
                                 alternativeTypes: Option[Set[Type]] = None
                                ) extends EnrichedProperty

// TODO: to develop
case class ObjectEnrichedProperty(name: String,
                                  `type`: Type = Type.Object,
                                  description: Option[String],
                                  optional: Boolean,
                                  alternativeTypes: Option[Set[Type]] = None
                                 ) extends EnrichedProperty

object EnrichedProperty {
  def build(name: String, schema: Schema, suggestedType: Option[Type] = None, alternativeTypes: Option[Set[Type]] = None, suggestedOptional: Boolean = false, required: Option[List[String]] = None): Option[EnrichedProperty] = {
    var optional = suggestedOptional || required.contains(name)
    val description: Option[String] = schema.description match {
      case Some(value) => Option(value.value)
      case None => None
    }
    suggestedType.getOrElse(schema.`type`.getOrElse(CommonProperties.Type.Null)) match {
      case Type.Null => None
      case Type.Boolean => Option(BooleanEnrichedProperty(name = name, description = description, optional = optional))
      case Type.String => {
        val maxLength = schema.maxLength match {
          case Some(value) => Option(value.value)
          case None => None
        }
        val minLength = schema.minLength match {
          case Some(value) => Option(value.value)
          case None => None
        }
        val pattern = schema.pattern match {
          case Some(value) => Option(value.value)
          case None => None
        }
        Option(StringEnrichedProperty(name = name, description = description, optional = optional, maxLength = maxLength, minLength = minLength, pattern = pattern, format = schema.format))
      }
      case Type.Integer => {
        val multipleOf = schema.multipleOf match {
          case Some(value) => value match {
            case MultipleOf.NumberMultipleOf(_) => None
            case MultipleOf.IntegerMultipleOf(value) => Option(value)
          }
          case None => None
        }
        val minimum = schema.minimum match {
          case Some(value) => value match {
            case Minimum.NumberMinimum(_) => None
            case Minimum.IntegerMinimum(value) => Option(value)
          }
          case None => None
        }
        val maximum = schema.maximum match {
          case Some(value) => value match {
            case Maximum.NumberMaximum(_) => None
            case Maximum.IntegerMaximum(value) => Option(value)
          }
          case None => None
        }
        Option(IntegerEnrichedProperty(name = name, description = description, optional = optional, multipleOf = multipleOf, minimum = minimum, maximum = maximum))
      }
      case Type.Number => {
        val multipleOf = schema.multipleOf match {
          case Some(value) => value match {
            case MultipleOf.NumberMultipleOf(value) => Option(value)
            case MultipleOf.IntegerMultipleOf(_) => None
          }
          case None => None
        }
        val minimum = schema.minimum match {
          case Some(value) => value match {
            case Minimum.NumberMinimum(value) => Option(value)
            case Minimum.IntegerMinimum(_) => None
          }
          case None => None
        }
        val maximum = schema.maximum match {
          case Some(value) => value match {
            case Maximum.NumberMaximum(value) => Option(value)
            case Maximum.IntegerMaximum(_) => None
          }
          case None => None
        }
        Option(NumberEnrichedProperty(name = name, description = description, optional = optional, multipleOf = multipleOf, minimum = minimum, maximum = maximum))
      }
      case Type.Array => Option(ArrayEnrichedProperty(name = name, description = description, optional = optional))
      case Type.Object => Option(ObjectEnrichedProperty(name = name, description = description, optional = optional))
      case Type.Union(value) => {
        optional = optional || value.contains(Type.Null)
        val types = value.filter(_ != Type.Null)
        if (types.isEmpty) {
          return None
        }
        build(name, schema, Option(types.head), Option(types.tail), optional)
      }
    }
  }
}
