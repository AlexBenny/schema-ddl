/*
 * Copyright (c) 2014-2021 Snowplow Analytics Ltd. All rights reserved.
 *
 * This program is licensed to you under the Apache License Version 2.0,
 * and you may not use this file except in compliance with the Apache License Version 2.0.
 * You may obtain a copy of the Apache License Version 2.0 at http://www.apache.org/licenses/LICENSE-2.0.
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the Apache License Version 2.0 is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the Apache License Version 2.0 for the specific language governing permissions and limitations there under.
 */
package com.snowplowanalytics.iglu.schemaddl.jsonschema.mutate

import cats.{Eq, Semigroup}
import cats.implicits._

// This library
import com.snowplowanalytics.iglu.schemaddl.jsonschema.Schema
import com.snowplowanalytics.iglu.schemaddl.jsonschema.properties._

object Widened {

  /**
   * Widens the properties of a schema.
   *
   * The expression `val a3 = Widened(a1, a2)` has the property that if data validates against either
   * schema a1 OR a2 then it also validates against schema a3.  Conversely, data that validates against
   * a3 does not necessarily validate against either a1 or a2.
   *
   * Widened is a [[cats.Semigroup]].
   */
  def apply(s1: Schema, s2: Schema): Schema = s1 |+| s2

  implicit val schemaSemigroup: Semigroup[Schema] = new Semigroup[Schema] {
    def combine(s1: Schema, s2: Schema): Schema = {
      val (items, additionalItems) = combineArrayItems(s1, s2)
      Schema(
        multipleOf = combineMultipleOf(s1, s2),
        minimum = combineMinimums(s1, s2),
        maximum = combineMaximums(s1, s2),

        maxLength = combineMaxLength(s1, s2),
        minLength = combineMinLength(s1, s2),
        pattern = combinePattern(s1, s2),
        format = combineFormat(s1, s2),
        `$schema` = combineSchemaUri(s1, s2),

        items = items,
        additionalItems = additionalItems,
        minItems = combineMinItems(s1, s2),
        maxItems = combineMaxItems(s1, s2),

        properties = combineProperties(s1, s2),
        additionalProperties = combineAdditionalProperties(s1, s2),
        required = combineRequired(s1, s2),
        patternProperties = combinePatternProperties(s1, s2),

        `type` = s1.`type` |+| s2.`type`,
        enum = s1.enum |+| s2.enum,
        oneOf = None, // TODO: we can do better than this when we have anyOf property.
        description = s1.description |+| s2.description
      )
    }
  }

  private def permitsType(s: Schema, t: Set[CommonProperties.Type]): Boolean =
    s.`type`.fold(true)(_.asUnion.value.intersect(t).nonEmpty)

  private def combineTypedProperties[A](s1: Schema,
                                s2: Schema,
                                forTypes: Set[CommonProperties.Type],
                                toProp: Schema => Option[A],
                                f: (A, A) => Option[A]): Option[A] =
    (permitsType(s1, forTypes), permitsType(s2, forTypes)) match {
      case (false, false) => None
      case (true, false) => toProp(s1)
      case (false, true) => toProp(s2)
      case (true, true) => (toProp(s1), toProp(s2)).mapN(f).flatten
    }

  private def combineNumberProperty[A <: NumberProperty](s1: Schema, s2: Schema, toProp: Schema => Option[A])(f: (A, A) => Option[A]): Option[A] =
    combineTypedProperties(s1, s2, Set(CommonProperties.Type.Number, CommonProperties.Type.Integer), toProp, f)

  private def combineStringProperty[A <: StringProperty](s1: Schema, s2: Schema, toProp: Schema => Option[A])(f: (A, A) => Option[A]): Option[A] =
    combineTypedProperties(s1, s2, Set(CommonProperties.Type.String), toProp, f)

  private def combineArrayProperty[A <: ArrayProperty](s1: Schema, s2: Schema, toProp: Schema => Option[A])(f: (A, A) => Option[A]): Option[A] =
    combineTypedProperties(s1, s2, Set(CommonProperties.Type.Array), toProp, f)

  private def combineObjectProperty[A <: ObjectProperty](s1: Schema, s2: Schema, toProp: Schema => Option[A])(f: (A, A) => Option[A]): Option[A] =
    combineTypedProperties(s1, s2, Set(CommonProperties.Type.Object), toProp, f)

  private def optionSemigroup[A](f: (A, A) => A): Semigroup[Option[A]] = new Semigroup[Option[A]] {
    def combine(o1: Option[A], o2: Option[A]): Option[A] =
      (o1, o2).mapN(f)
  }

  private def combineMinimums(s1: Schema, s2: Schema): Option[NumberProperty.Minimum] =
    combineNumberProperty(s1, s2, _.minimum) {
      case (m1, m2) => Some(if (m1.getAsDecimal < m2.getAsDecimal) m1 else m2)
    }

  private def combineMaximums(s1: Schema, s2: Schema): Option[NumberProperty.Maximum] =
    combineNumberProperty(s1, s2, _.maximum) {
      case (m1, m2) => Some(if (m1.getAsDecimal > m2.getAsDecimal) m1 else m2)
    }

  private def combineMultipleOf(s1: Schema, s2: Schema): Option[NumberProperty.MultipleOf] =
    combineNumberProperty(s1, s2, _.multipleOf) {
      case (p1 @ NumberProperty.MultipleOf.IntegerMultipleOf(v1), NumberProperty.MultipleOf.IntegerMultipleOf(v2)) =>
        if (v1 === v2) Some(p1) else None
      case (p1, p2) =>
        if (p1.getAsDecimal === p2.getAsDecimal) Some(p1) else None
    }

  private def combineMaxLength(s1: Schema, s2: Schema): Option[StringProperty.MaxLength] =
    combineStringProperty(s1, s2, _.maxLength) {
      case (m1, m2) => Some(StringProperty.MaxLength(m1.value.max(m2.value)))
    }

  private def combineMinLength(s1: Schema, s2: Schema): Option[StringProperty.MinLength] =
    combineStringProperty(s1, s2, _.minLength) {
      case (m1, m2) => Some(StringProperty.MinLength(m1.value.min(m2.value)))
    }

  private def combinePattern(s1: Schema, s2: Schema): Option[StringProperty.Pattern] =
    combineStringProperty(s1, s2, _.pattern) {
      case (p1, p2) => if (p1.value === p2.value) Some(p1) else None
    }

  private def combineFormat(s1: Schema, s2: Schema): Option[StringProperty.Format] = {
    implicit val _ = Eq.fromUniversalEquals[StringProperty.Format]
    combineStringProperty(s1, s2, _.format) {
      case (p1, p2) => if (p1 === p2) Some(p1) else None
    }
  }

  private def combineSchemaUri(s1: Schema, s2: Schema): Option[StringProperty.SchemaUri] = {
    implicit val _ = Eq.fromUniversalEquals[StringProperty.SchemaUri]
    combineStringProperty(s1, s2, _.`$schema`) {
      case (p1, p2) => if (p1 === p2) Some(p1) else None
    }
  }

  private implicit val typeSemigroup: Semigroup[Option[CommonProperties.Type]] = optionSemigroup[CommonProperties.Type] {
    case (t1, t2) =>
      (t1.asUnion.value ++ t2.asUnion.value).toList match {
        case single :: Nil => single
        case more => CommonProperties.Type.Union(more.toSet)
      }
  }

  private implicit val enumSemigroup: Semigroup[Option[CommonProperties.Enum]] = optionSemigroup[CommonProperties.Enum] {
    case (t1, t2) => CommonProperties.Enum(t1.value ++ t2.value)
  }

  private implicit val descriptionSemigroup: Semigroup[Option[CommonProperties.Description]] = new Semigroup[Option[CommonProperties.Description]] {
    implicit val _ = Eq.fromUniversalEquals[CommonProperties.Description]
    def combine(o1: Option[CommonProperties.Description], o2: Option[CommonProperties.Description]): Option[CommonProperties.Description] =
      (o1, o2).mapN {
        case (d1, d2) => if (d1 === d2) Some(d1) else None
      }.flatten
  }

  private def combineArrayItems(s1: Schema, s2: Schema): (Option[ArrayProperty.Items], Option[ArrayProperty.AdditionalItems]) =
    (permitsType(s1, Set(CommonProperties.Type.Array)), permitsType(s2, Set(CommonProperties.Type.Array))) match {
      case (false, false) => (None, None)
      case (true, false) => (s1.items, s1.additionalItems)
      case (false, true) => (s2.items, s2.additionalItems)
      case (true, true) =>
        import ArrayProperty.Items._
        import ArrayProperty.AdditionalItems._
        (s1.items, s2.items) match {
          case (i1, None) => (i1, s1.additionalItems)
          case (None, i2) => (i2, s2.additionalItems)
          case (Some(ListItems(li1)), Some(ListItems(li2))) => (Some(ListItems(li1 |+| li2)), None)

          case (Some(ListItems(li1)), Some(TupleItems(ti2))) =>
            val ai = s2.additionalItems match {
              case Some(AdditionalItemsSchema(ais2)) => AdditionalItemsSchema(ais2 |+| li1)
              case Some(AdditionalItemsAllowed(false)) => AdditionalItemsSchema(li1)
              case Some(AdditionalItemsAllowed(true)) | None => AdditionalItemsAllowed(true)
            }
            (Some(TupleItems(ti2.map(_ |+| li1))), Some(ai))

          case (Some(TupleItems(ti1)), Some(ListItems(li2))) =>
            val ai = s1.additionalItems match {
              case Some(AdditionalItemsSchema(ais1)) => AdditionalItemsSchema(ais1 |+| li2)
              case Some(AdditionalItemsAllowed(false)) => AdditionalItemsSchema(li2)
              case Some(AdditionalItemsAllowed(true)) | None => AdditionalItemsAllowed(true)
            }
            (Some(TupleItems(ti1.map(_ |+| li2))), Some(ai))

          case (Some(TupleItems(ti1)), Some(TupleItems(ti2))) if ti1.size === ti2.size =>
            val ai = (s1.additionalItems, s2.additionalItems) match {
              case (None | Some(AdditionalItemsAllowed(true)), _) => Some(AdditionalItemsAllowed(true))
              case (_, None | Some(AdditionalItemsAllowed(true))) => Some(AdditionalItemsAllowed(true))
              case (ai1, Some(AdditionalItemsAllowed(false))) => ai1
              case (Some(AdditionalItemsAllowed(false)), ai2) => ai2
              case (Some(AdditionalItemsSchema(ais1)), Some(AdditionalItemsSchema(ais2))) => Some(AdditionalItemsSchema(ais1 |+| ais2))
            }
            (Some(TupleItems(ti1.zip(ti2).map { case (i1, i2) => i1 |+| i2 })), ai)

          case (Some(TupleItems(ti1)), Some(TupleItems(ti2))) =>
            val toMerge = (s1.additionalItems, s2.additionalItems) match {
              case (None | Some(AdditionalItemsAllowed(true)), _) => Nil
              case (_, None | Some(AdditionalItemsAllowed(true))) => Nil
              case (Some(AdditionalItemsAllowed(false)), Some(AdditionalItemsAllowed(false))) => ti1 ++ ti2
              case (Some(AdditionalItemsAllowed(false)), Some(AdditionalItemsSchema(ais2))) => ais2 :: ti1 ++ ti2
              case (Some(AdditionalItemsSchema(ais1)), Some(AdditionalItemsAllowed(false))) => ais1 :: ti1 ++ ti2
              case (Some(AdditionalItemsSchema(ais1)), Some(AdditionalItemsSchema(ais2))) => ais1 :: ais2 :: ti1 ++ ti2
            }
            (Some(ListItems(toMerge.foldLeft(Schema())(_ |+| _))), None)
        }
    }

  private def combineMinItems(s1: Schema, s2: Schema): Option[ArrayProperty.MinItems] =
    combineArrayProperty(s1, s2, _.minItems) {
      case (m1, m2) => Some(ArrayProperty.MinItems(m1.value.min(m2.value)))
    }

  private def combineMaxItems(s1: Schema, s2: Schema): Option[ArrayProperty.MaxItems] =
    combineArrayProperty(s1, s2, _.maxItems) {
      case (m1, m2) => Some(ArrayProperty.MaxItems(m1.value.max(m2.value)))
    }

  private def combineAdditionalProperties(s1: Schema, s2: Schema): Option[ObjectProperty.AdditionalProperties] =
    combineObjectProperty(s1, s2, _.additionalProperties) {
      case (ObjectProperty.AdditionalProperties.AdditionalPropertiesAllowed(true), _) | (_, ObjectProperty.AdditionalProperties.AdditionalPropertiesAllowed(true)) =>
        Some(ObjectProperty.AdditionalProperties.AdditionalPropertiesAllowed(true))
      case (ObjectProperty.AdditionalProperties.AdditionalPropertiesAllowed(false), p2) => Some(p2)
      case (p1, ObjectProperty.AdditionalProperties.AdditionalPropertiesAllowed(false)) => Some(p1)
      case (ObjectProperty.AdditionalProperties.AdditionalPropertiesSchema(s1), ObjectProperty.AdditionalProperties.AdditionalPropertiesSchema(s2)) =>
        Some(ObjectProperty.AdditionalProperties.AdditionalPropertiesSchema(s1 |+| s2))
    }

  private def combineRequired(s1: Schema, s2: Schema): Option[ObjectProperty.Required] =
    combineObjectProperty(s1, s2, _.required) {
      case (p1, p2) => Some(ObjectProperty.Required((p1.value.toSet.intersect(p2.value.toSet)).toList))
    }

  private def combineMaps(m1: Map[String, Schema],
                          m2: Map[String, Schema],
                          ap1: Option[ObjectProperty.AdditionalProperties],
                          ap2: Option[ObjectProperty.AdditionalProperties]): Map[String, Schema] =
    (m1.keySet ++ m2.keySet).toList.map { k =>
      val fixed = (m1.get(k), m2.get(k)) match {
        case (Some(prop1), Some(prop2)) => prop1 |+| prop2
        case (Some(prop1), None) =>
          ap2 match {
            case Some(ObjectProperty.AdditionalProperties.AdditionalPropertiesSchema(aps)) => aps |+| prop1
            case Some(ObjectProperty.AdditionalProperties.AdditionalPropertiesAllowed(false)) => prop1
            case Some(ObjectProperty.AdditionalProperties.AdditionalPropertiesAllowed(true)) => Schema()
            case None => Schema()
          }
        case (None, Some(prop2)) =>
          ap1 match {
            case Some(ObjectProperty.AdditionalProperties.AdditionalPropertiesSchema(aps)) => aps |+| prop2
            case Some(ObjectProperty.AdditionalProperties.AdditionalPropertiesAllowed(false)) => prop2
            case Some(ObjectProperty.AdditionalProperties.AdditionalPropertiesAllowed(true)) => Schema()
            case None => Schema()
          }
        case (None, None) => throw new IllegalStateException
      }
      k -> fixed
    }.toMap

  private def combinePatternProperties(s1: Schema, s2: Schema): Option[ObjectProperty.PatternProperties] = {
    if (s1.patternProperties.isEmpty && s2.patternProperties.isEmpty)
      None
    else {
      val props1 = s1.patternProperties.map(_.value).getOrElse(Map.empty)
      val props2 = s2.patternProperties.map(_.value).getOrElse(Map.empty)
      Some(ObjectProperty.PatternProperties {
        combineMaps(props1, props2, s1.additionalProperties, s2.additionalProperties)
      })
    }
  }

  private def combineProperties(s1: Schema, s2: Schema): Option[ObjectProperty.Properties] = {
    if (s1.properties.isEmpty && s2.properties.isEmpty)
      None
    else {
      val props1 = s1.properties.map(_.value).getOrElse(Map.empty)
      val props2 = s2.properties.map(_.value).getOrElse(Map.empty)
      Some(ObjectProperty.Properties {
        combineMaps(props1, props2, s1.additionalProperties, s2.additionalProperties)
      })
    }
  }
}
