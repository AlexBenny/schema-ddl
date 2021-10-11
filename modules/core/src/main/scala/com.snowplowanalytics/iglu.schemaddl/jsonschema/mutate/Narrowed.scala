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

import cats.Semigroup
import cats.implicits._

// This library
import com.snowplowanalytics.iglu.schemaddl.jsonschema.Schema
import com.snowplowanalytics.iglu.schemaddl.jsonschema.properties._


object Narrowed {

  /**
   * Narrows the properties of a schema.
   *
   * The expression `val a3 = Narrowed(a1, a2)` has the property that if data validates against both
   * schemas a1 AND a2 then it also validates against schema a3.  Conversely, data that validates
   * against a3 does not necessarily validate against both a1 and a2.
   *
   * Narrowed is a [[cats.Semigroup]]. Note order can be important: `Narrowed(s1, s2)` and
   * `Narrowed(s2, s1)` can be subtly different.
   */
  def apply(s1: Schema, s2: Schema): Schema = s1 |+| s2

  implicit val schemaSemigroup: Semigroup[Schema] = new Semigroup[Schema] {
    def combine(s1: Schema, s2: Schema): Schema =
      Schema(
        multipleOf = s1.multipleOf |+| s2.multipleOf,
        minimum = s1.minimum |+| s2.minimum,
        maximum = s1.maximum |+| s2.maximum,

        maxLength = s1.maxLength |+| s2.maxLength,
        minLength = s1.minLength |+| s2.minLength,
        pattern = s1.pattern |+| s2.pattern,
        format = s1.format |+| s2.format,
        `$schema` = s1.`$schema` |+| s2.`$schema`,

        items = s1.items |+| s2.items,
        additionalItems = s1.additionalItems |+| s2.additionalItems,
        minItems = s1.minItems |+| s2.minItems,
        maxItems = s1.maxItems |+| s2.maxItems,

        properties = s1.properties |+| s2.properties,
        additionalProperties = s1.additionalProperties |+| s2.additionalProperties,
        required = s1.required |+| s2.required,
        patternProperties = s1.patternProperties |+| s2.patternProperties,

        `type` = s1.`type` |+| s2.`type`,
        enum = s1.enum |+| s2.enum,
        oneOf = s1.oneOf |+| s2.oneOf,
        description = s1.description |+| s2.description
      )
  }

  private implicit val minimumsSemigroup: Semigroup[NumberProperty.Minimum] = new Semigroup[NumberProperty.Minimum] {
    def combine(m1: NumberProperty.Minimum, m2: NumberProperty.Minimum): NumberProperty.Minimum =
      if (m1.getAsDecimal > m2.getAsDecimal) m1 else m2
  }

  private implicit val maximumsSemigroup: Semigroup[NumberProperty.Maximum] = new Semigroup[NumberProperty.Maximum] {
    def combine(m1: NumberProperty.Maximum, m2: NumberProperty.Maximum): NumberProperty.Maximum =
      if (m1.getAsDecimal < m2.getAsDecimal) m1 else m2
  }

  private implicit val multipleOfSemigroup: Semigroup[NumberProperty.MultipleOf] = new Semigroup[NumberProperty.MultipleOf] {
    def combine(m1: NumberProperty.MultipleOf, m2: NumberProperty.MultipleOf): NumberProperty.MultipleOf =
      (m1, m2) match {
        case (NumberProperty.MultipleOf.NumberMultipleOf(v1), _) => NumberProperty.MultipleOf.NumberMultipleOf(v1.min(m2.getAsDecimal))
        case (_, NumberProperty.MultipleOf.NumberMultipleOf(v2)) => NumberProperty.MultipleOf.NumberMultipleOf(v2.min(m1.getAsDecimal))
        case (NumberProperty.MultipleOf.IntegerMultipleOf(v1), NumberProperty.MultipleOf.IntegerMultipleOf(v2)) => NumberProperty.MultipleOf.IntegerMultipleOf(v1.min(v2))
      }
  }

  private implicit val maxLengthSemigroup: Semigroup[StringProperty.MaxLength] = new Semigroup[StringProperty.MaxLength] {
    def combine(m1: StringProperty.MaxLength, m2: StringProperty.MaxLength): StringProperty.MaxLength =
      StringProperty.MaxLength(m1.value.min(m2.value))
  }

  private implicit val minLengthSemigroup: Semigroup[StringProperty.MinLength] = new Semigroup[StringProperty.MinLength] {
    def combine(m1: StringProperty.MinLength, m2: StringProperty.MinLength): StringProperty.MinLength =
      StringProperty.MinLength(m1.value.max(m2.value))
  }

  private def takeFirstSemigroup[A]: Semigroup[Option[A]] = new Semigroup[Option[A]] {
    def combine(o1: Option[A], o2: Option[A]): Option[A] =
      o1.orElse(o2)
  }

  private implicit val patternSemigroup: Semigroup[Option[StringProperty.Pattern]] =
    takeFirstSemigroup[StringProperty.Pattern]

  private implicit val formatSemigroup: Semigroup[Option[StringProperty.Format]] =
    takeFirstSemigroup[StringProperty.Format]

  private implicit val schemaUriSemigroup: Semigroup[Option[StringProperty.SchemaUri]] =
    takeFirstSemigroup[StringProperty.SchemaUri]

  private implicit val propertiesSemigroup: Semigroup[ObjectProperty.Properties] = new Semigroup[ObjectProperty.Properties] {
    def combine(t1: ObjectProperty.Properties, t2: ObjectProperty.Properties): ObjectProperty.Properties =
      ObjectProperty.Properties(t1.value |+| t2.value)
  }

  private implicit val typesSemigroup: Semigroup[CommonProperties.Type] = new Semigroup[CommonProperties.Type] {
    def combine(t1: CommonProperties.Type, t2: CommonProperties.Type): CommonProperties.Type =
      t1.asUnion.value.intersect(t2.asUnion.value).toList match {
        case single :: Nil => single
        case more => CommonProperties.Type.Union(more.toSet)
      }
  }

  private implicit val enumsSemigroup: Semigroup[CommonProperties.Enum] = new Semigroup[CommonProperties.Enum] {
    def combine(t1: CommonProperties.Enum, t2: CommonProperties.Enum): CommonProperties.Enum =
      CommonProperties.Enum(t1.value.intersect(t2.value))
  }

  private implicit val oneOfSemigroup: Semigroup[CommonProperties.OneOf] = new Semigroup[CommonProperties.OneOf] {
    def combine(t1: CommonProperties.OneOf, t2: CommonProperties.OneOf): CommonProperties.OneOf =
      CommonProperties.OneOf(t1.value.flatMap(schema1 => t2.value.map(_ |+| schema1).toSet.toList))
  }

  private implicit val itemsSemigroup: Semigroup[ArrayProperty.Items] = new Semigroup[ArrayProperty.Items] {
    def combine(p1: ArrayProperty.Items, p2: ArrayProperty.Items): ArrayProperty.Items =
      (p1, p2) match {
        case (ArrayProperty.Items.ListItems(s1), ArrayProperty.Items.ListItems(s2)) =>
          ArrayProperty.Items.ListItems(s1 |+| s2)
        case (ArrayProperty.Items.ListItems(s1), ArrayProperty.Items.TupleItems(tupled)) =>
          ArrayProperty.Items.TupleItems(tupled.map(_ |+| s1))
        case (ArrayProperty.Items.TupleItems(tupled), ArrayProperty.Items.ListItems(s2)) =>
          ArrayProperty.Items.TupleItems(tupled.map(_ |+| s2))
        case (ArrayProperty.Items.TupleItems(tupled1), ArrayProperty.Items.TupleItems(tupled2)) =>
          def go(acc: List[Schema], tupled1: List[Schema], tupled2: List[Schema]): List[Schema] =
            (tupled1, tupled2) match {
              case (Nil, Nil) => acc
              case (h1 :: t1, Nil) => go(h1 :: acc, t1, Nil)
              case (Nil, h2 :: t2) => go(h2 :: acc, t2, Nil)
              case (h1 :: t1, h2 :: t2) => go((h1 |+| h2) :: acc, t1, t2)
            }
          ArrayProperty.Items.TupleItems(go(Nil, tupled1, tupled2).reverse)
      }
  }

  private implicit def additionalItemsSemigroup: Semigroup[ArrayProperty.AdditionalItems] = new Semigroup[ArrayProperty.AdditionalItems]{
    def combine(p1: ArrayProperty.AdditionalItems, p2: ArrayProperty.AdditionalItems): ArrayProperty.AdditionalItems =
      (p1, p2) match {
        case (ArrayProperty.AdditionalItems.AdditionalItemsAllowed(false), _) | (_, ArrayProperty.AdditionalItems.AdditionalItemsAllowed(false)) =>
          ArrayProperty.AdditionalItems.AdditionalItemsAllowed(false)
        case (ArrayProperty.AdditionalItems.AdditionalItemsAllowed(true), _) => p2
        case (_, ArrayProperty.AdditionalItems.AdditionalItemsAllowed(true)) => p1
        case (ArrayProperty.AdditionalItems.AdditionalItemsSchema(s1), ArrayProperty.AdditionalItems.AdditionalItemsSchema(s2)) =>
          ArrayProperty.AdditionalItems.AdditionalItemsSchema(s1 |+| s2)
      }
  }

  private implicit val minItemsSemigroup: Semigroup[ArrayProperty.MinItems] = new Semigroup[ArrayProperty.MinItems] {
    def combine(m1: ArrayProperty.MinItems, m2: ArrayProperty.MinItems): ArrayProperty.MinItems =
      ArrayProperty.MinItems(m1.value.max(m2.value))
  }

  private implicit val maxItemsSemigroup: Semigroup[ArrayProperty.MaxItems] = new Semigroup[ArrayProperty.MaxItems] {
    def combine(m1: ArrayProperty.MaxItems, m2: ArrayProperty.MaxItems): ArrayProperty.MaxItems =
      ArrayProperty.MaxItems(m1.value.min(m2.value))
  }

  private implicit def additionalPropertiesSemigroup: Semigroup[ObjectProperty.AdditionalProperties] = new Semigroup[ObjectProperty.AdditionalProperties]{
    def combine(p1: ObjectProperty.AdditionalProperties, p2: ObjectProperty.AdditionalProperties): ObjectProperty.AdditionalProperties =
      (p1, p2) match {
        case (ObjectProperty.AdditionalProperties.AdditionalPropertiesAllowed(false), _) | (_, ObjectProperty.AdditionalProperties.AdditionalPropertiesAllowed(false)) =>
          ObjectProperty.AdditionalProperties.AdditionalPropertiesAllowed(false)
        case (ObjectProperty.AdditionalProperties.AdditionalPropertiesAllowed(true), _) => p2
        case (_, ObjectProperty.AdditionalProperties.AdditionalPropertiesAllowed(true)) => p1
        case (ObjectProperty.AdditionalProperties.AdditionalPropertiesSchema(s1), ObjectProperty.AdditionalProperties.AdditionalPropertiesSchema(s2)) =>
          ObjectProperty.AdditionalProperties.AdditionalPropertiesSchema(s1 |+| s2)
      }
  }

  private implicit def requiredSemigroup: Semigroup[ObjectProperty.Required] = new Semigroup[ObjectProperty.Required]{
    def combine(p1: ObjectProperty.Required, p2: ObjectProperty.Required): ObjectProperty.Required =
      ObjectProperty.Required((p1.value ++ p2.value).toSet.toList)
  }

  private implicit def patternPropertiesSemigroup: Semigroup[ObjectProperty.PatternProperties] = new Semigroup[ObjectProperty.PatternProperties]{
    def combine(p1: ObjectProperty.PatternProperties, p2: ObjectProperty.PatternProperties): ObjectProperty.PatternProperties =
      ObjectProperty.PatternProperties(p1.value |+| p2.value)
  }

  private implicit val descriptionSemigroup: Semigroup[Option[CommonProperties.Description]] =
    takeFirstSemigroup[CommonProperties.Description]

}
