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

// This library
import com.snowplowanalytics.iglu.schemaddl.jsonschema.Schema
import com.snowplowanalytics.iglu.schemaddl.jsonschema.properties._

object Mutate {

  /**
  * Weakens a schema by merging `oneOf` sub-schemas into the parent schema.
  * 
  * Any data that validates against the input schema will also validate against the output schema.
  * Conversely, data that validates against the output schema will not necessarily validate against
  * the input schema.  In this sense, the schema has been weakened.
  *
  * The output schema is helpful as an alternative description of the data structure, especially for
  * use cases that ignore the "oneOf" annotations.
   */
  def weaken(schema: Schema): Schema = {
    val weakenedProps = schema.copy(
      items = schema.items.map {
        case ArrayProperty.Items.ListItems(li) => ArrayProperty.Items.ListItems(weaken(li))
        case ArrayProperty.Items.TupleItems(ti) => ArrayProperty.Items.TupleItems(ti.map(weaken))
      },
      properties = schema.properties.map(properties  => ObjectProperty.Properties(properties.value.mapValues(weaken))),
      patternProperties = schema.patternProperties.map(pp  => ObjectProperty.PatternProperties(pp.value.mapValues(weaken))),
      additionalProperties = schema.additionalProperties.map {
        case ObjectProperty.AdditionalProperties.AdditionalPropertiesSchema(aps) => ObjectProperty.AdditionalProperties.AdditionalPropertiesSchema(weaken(aps))
        case other => other
      },
      additionalItems = schema.additionalItems.map {
        case ArrayProperty.AdditionalItems.AdditionalItemsSchema(ais) => ArrayProperty.AdditionalItems.AdditionalItemsSchema(weaken(ais))
        case other => other
      },
      oneOf = None
    )
    schema
      .oneOf
      .toList
      .flatMap(_.value)
      .map(weaken)
      .map(Narrowed(weakenedProps, _))
      .reduceOption(Widened(_, _))
      .getOrElse(weakenedProps)
  }

  /**
   * Alters a schema to disallow additional properties or additional items.
   *
   * Data that validates against the output schema will also validate against the input schema.
   * Conversely, data that validates against the input schema will not necessarily validate against
   * the output schema.
   */
  def dropAdditionals(schema: Schema): Schema =
    schema.copy(
      items = schema.items.map {
        case ArrayProperty.Items.ListItems(li) => ArrayProperty.Items.ListItems(dropAdditionals(li))
        case ArrayProperty.Items.TupleItems(ti) => ArrayProperty.Items.TupleItems(ti.map(dropAdditionals))
      },
      properties = schema.properties.map(properties => ObjectProperty.Properties(properties.value.mapValues(dropAdditionals))),
      oneOf = schema.oneOf.map(oneOf => CommonProperties.OneOf(oneOf.value.map(dropAdditionals))),
      additionalProperties = Some(ObjectProperty.AdditionalProperties.AdditionalPropertiesAllowed(false)),
      additionalItems = Some(ArrayProperty.AdditionalItems.AdditionalItemsAllowed(false))
    )

}
