/*
 * Copyright (c) 2018-2021 Snowplow Analytics Ltd. All rights reserved.
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

import com.snowplowanalytics.iglu.schemaddl.SpecHelpers

class MutateSpec extends org.specs2.Specification {

  def is = s2"""
    weaken should not make any changes to a simpe schema $common1
    weaken should merge oneOf string/number into a union type $common2
    weaken should merge oneOf enums into an extended enum $common2

    NUMBERS

    weaken should take widest minimum and maximum from oneOf fields $numbers1
    weaken should take narrowest minimum and maximum between parent and oneOf field $numbers2a $numbers2b
    weaken should drop incompatible multipleOfs $numbers3

    STRINGS

    weaken should take widest minLength and maxLength from oneOf fields $strings1
    weaken should take narrowest minLength and maxLength between parent and oneOf field $strings2a $strings2b
    weaken should preserve compatible pattern and format $strings3
    weaken should drop incompatible pattern and format $strings4

    OBJECTS

    weaken should merge oneOf object properties into a common object type $object1
    weaken should preserve property types when additionalProperties is used to forbid alternatives $object2
    weaken should merge common oneOf properties into a union type property $object3
    weaken should take widest minLength and maxLength from common stringy fields $object4
    weaken should take widest minimum and maximum from common numeric fields $object5
    weaken should filter required fields from oneOf properties $object6
    weaken should preserve common oneOf pattern properties $object7
    weaken should merge oneOf object patternProperties into a common object type $object8
    weaken should preserve patternProperty types when additionalProperties is used to forbid alternatives $object9
    weaken should merge common oneOf patternProperties into a union type property $object10
    weaken should merge enums of a common oneOf property $object11
    weaken should preserve properties and patternProperties from oneOf $object12

    ARRAYS

    weaken should merge oneOf items into a union type item $array1
    weaken should take widest minItems and maxItems from oneOf alternatives $array2
    weaken should merge unmatched oneOf tupled-items into the most permissive type $array3
    """
  def common1 = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        |"type": ["object", "null"],
        | "description": "simple schema",
        | "properties": {
        |   "numeric": {
        |     "description": "this is a number",
        |     "type": "number",
        |     "minimum": 150,
        |     "maximum": 200,
        |     "multipleOf": 10
        |   },
        |   "stringy": {
        |     "type": "string",
        |     "minLength": 150,
        |     "maxLength": 200,
        |     "pattern": "^[a-z0-9-]*$",
        |     "format": "uuid"
        |   },
        |   "listy": {
        |     "type": "array",
        |     "items": [{"type": "string"}, {"type": "number"}],
        |     "additionalItems": false,
        |     "minItems": 2,
        |     "minItems": 2
        |   },
        |   "status": {
        |     "enum": ["HAPPY", "SAD"]
        |   }
        | },
        | "additionalProperties": true,
        | "required": ["numeric", "stringy"],
        | "patternProperties": {
        |   "^S_": {"type": "number"}
        | }
        |}
      """.stripMargin)

    Mutate.weaken(input) must_== input
  }

  def common2 = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        | "oneOf": [
        |   {"type": "string"},
        |   {"type": "number"}
        | ]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"type": ["string", "number"]
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }

  def common3 = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        | "oneOf": [
        |   {"enum": ["x", "y", "z"]},
        |   {"enum": ["a", "b", "c"]}
        | ]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"enum": ["x", "y", "z", "a", "b", "c"]
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }


  def object1 = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        |"type": "object",
        |"oneOf": [
        |  {
        |    "properties": {"a": {"type": "string"}}
        |  },
        |  {
        |    "properties": {"b": {"type": "string"}}
        |  }
        |]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"type": "object",
        |"properties": {
        |  "a": {},
        |  "b": {}
        |}
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }

  def object2 = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        |"type": "object",
        |"oneOf": [
        |  {
        |    "properties": {"a": {"type": "string"}},
        |    "additionalProperties": false
        |  },
        |  {
        |    "properties": {"b": {"type": "string"}}
        |  }
        |]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"type": "object",
        |"properties": {
        |  "a": {},
        |  "b": {"type": "string"}
        |}
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }

  def object3 = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        |"type": "object",
        |"oneOf": [
        |  {
        |    "properties": {"a": {"type": "string"}}
        |  },
        |  {
        |    "properties": {"a": {"type": "number"}}
        |  }
        |]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"type": "object",
        |"properties": {
        |  "a": {"type": ["string", "number"]}
        |}
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }

  def object4 = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        |"type": "object",
        |"additionalProperties": false,
        |"oneOf": [
        |  {
        |    "properties": {
        |      "stringy": {
        |        "type": "string",
        |        "minLength": 10,
        |        "maxLength": 100
        |      }
        |    }
        |  },
        |  {
        |    "properties": {
        |      "stringy": {
        |        "type": "string",
        |        "minLength": 5,
        |        "maxLength": 105
        |      }
        |    }
        |  }
        |]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"type": "object",
        |"additionalProperties": false,
        |"properties": {
        |  "stringy": {
        |    "type": "string",
        |    "minLength": 5,
        |    "maxLength": 105
        |  }
        |}
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }

  def object5 = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        |"type": "object",
        |"additionalProperties": false,
        |"oneOf": [
        |  {
        |    "properties": {
        |      "numeric": {
        |        "type": "integer",
        |        "minimum": 10,
        |        "maximum": 100
        |      }
        |    }
        |  },
        |  {
        |    "properties": {
        |      "numeric": {
        |        "type": "integer",
        |        "minimum": 5,
        |        "maximum": 105
        |      }
        |    }
        |  }
        |]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"type": "object",
        |"additionalProperties": false,
        |"properties": {
        |  "numeric": {
        |    "type": "integer",
        |    "minimum": 5,
        |    "maximum": 105
        |  }
        |}
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }

  def object6 = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        |"type": "object",
        |"additionalProperties": false,
        |"oneOf": [
        |  {
        |    "properties": {
        |      "a": {"type": "string"},
        |      "b": {"type": "string"}
        |    },
        |    "required": ["a", "b"]
        |  },
        |  {
        |    "properties": {
        |      "a": {"type": "string"},
        |      "b": {"type": "string"}
        |    },
        |    "required": ["a"]
        |  },
        |  {
        |    "properties": {
        |      "a": {"type": "string"},
        |      "b": {"type": "string"},
        |      "c": {"type": "string"}
        |    },
        |    "required": ["a", "c"]
        |  }
        |]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"type": "object",
        |"additionalProperties": false,
        |"properties": {
        |  "a": {"type": "string"},
        |  "b": {"type": "string"},
        |  "c": {"type": "string"}
        |},
        |"required": ["a"]
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }

  def object7 = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        |"type": "object",
        |"oneOf": [
        |  {
        |    "patternProperties": {
        |      "^A_": {"type": "number"}
        |    }
        |  },
        |  {
        |    "patternProperties": {
        |      "^A_": {"type": "number"}
        |    }
        |  }
        |]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"type": "object",
        |"patternProperties": {
        |  "^A_": {"type": "number"}
        |}
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }

  def object8 = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        |"type": "object",
        |"oneOf": [
        |  {
        |    "patternProperties": {"^A_": {"type": "string"}}
        |  },
        |  {
        |    "patternProperties": {"^B_": {"type": "string"}}
        |  }
        |]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"type": "object",
        |"patternProperties": {
        |  "^A_": {},
        |  "^B_": {}
        |}
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }

  def object9 = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        |"type": "object",
        |"oneOf": [
        |  {
        |    "patternProperties": {"^A_": {"type": "string"}},
        |    "additionalProperties": false
        |  },
        |  {
        |    "patternProperties": {"^B_": {"type": "string"}}
        |  }
        |]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"type": "object",
        |"patternProperties": {
        |  "^A_": {},
        |  "^B_": {"type": "string"}
        |}
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }

  def object10 = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        |"type": "object",
        |"oneOf": [
        |  {
        |    "patternProperties": {"^A_": {"type": "string"}}
        |  },
        |  {
        |    "patternProperties": {"^A_": {"type": "number"}}
        |  }
        |]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"type": "object",
        |"patternProperties": {
        |  "^A_": {"type": ["string", "number"]}
        |}
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }

  def object11 = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        |"type": "object",
        |"oneOf": [
        |  {
        |    "properties": {"a": {"enum": ["x", "y", "z"]}}
        |  },
        |  {
        |    "properties": {"a": {"enum": ["a", "b", "c"]}}
        |  }
        |]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"type": "object",
        |"properties": {
        |  "a": {"enum": ["x", "y", "z", "a", "b", "c"]}
        |}
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }

  def object12 = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        |"type": "object",
        |"additionalProperties": false,
        |"oneOf": [
        |  {
        |    "properties": {"a": {"type": "string"}}
        |  },
        |  {
        |    "patternProperties": {"a": {"type": "string"}}
        |  }
        |]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"type": "object",
        |"additionalProperties": false,
        |"properties": {"a": {"type": "string"}},
        |"patternProperties": {"a": {"type": "string"}}
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }

  def array1 = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        |"type": "array",
        |"oneOf": [
        |  {
        |    "items": {"type": "string"}
        |  },
        |  {
        |    "items": {"type": "number"}
        |  }
        |]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"type": "array",
        |"items": {"type": ["string", "number"]}
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }

  def array2 = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        |"type": "array",
        |"oneOf": [
        |  {
        |    "maxItems": 42,
        |    "minItems": 10
        |  },
        |  {
        |    "maxItems": 100,
        |    "minItems": 2
        |  }
        |]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"type": "array",
        |"maxItems": 100,
        |"minItems": 2
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }

  def array3 = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        |"type": "array",
        |"oneOf": [
        |  {
        |    "items": [
        |      {"type": "string"},
        |      {"type": "string"}
        |    ]
        |  },
        |  {
        |    "items": [
        |      {"type": "string"}
        |    ]
        |  }
        |]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"type": "array",
        |"items": {}
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }

  def numbers1 = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        |"type": "number",
        |"oneOf": [
        |  {
        |    "multipleOf": 0.3,
        |    "minimum": 0.1,
        |    "maximum": 100
        |  },
        |  {
        |    "multipleOf": 0.3,
        |    "minimum": 42.0,
        |    "maximum": 123.45
        |  }
        |]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"type": "number",
        |"multipleOf": 0.3,
        |"minimum": 0.1,
        |"maximum": 123.45
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }

  def numbers2a = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        |"type": "number",
        |"multipleOf": 0.3,
        |"minimum": 0.1,
        |"maximum": 12345,
        |"oneOf": [
        |  {
        |    "multipleOf": 0.3,
        |    "minimum": 42.0,
        |    "maximum": 1000
        |  }
        |]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"type": "number",
        |"multipleOf": 0.3,
        |"minimum": 42.0,
        |"maximum": 1000
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }

  def numbers2b = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        |"type": "number",
        |"multipleOf": 0.3,
        |"minimum": 42.0,
        |"maximum": 1000,
        |"oneOf": [
        |  {
        |    "multipleOf": 0.3,
        |    "minimum": 0.1,
        |    "maximum": 12345
        |  }
        |]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"type": "number",
        |"multipleOf": 0.3,
        |"minimum": 42.0,
        |"maximum": 1000
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }

  def numbers3 = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        |"type": "number",
        |"oneOf": [
        |  {
        |    "multipleOf": 0.3
        |  },
        |  {
        |    "multipleOf": 0.7
        |  }
        |]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"type": "number"
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }

  def strings1 = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        |"type": "string",
        |"oneOf": [
        |  {
        |    "minLength": 5,
        |    "maxLength": 105
        |  },
        |  {
        |    "minLength": 10,
        |    "maxLength": 90
        |  }
        |]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"type": "string",
        |"minLength": 5,
        |"maxLength": 105
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }

  def strings2a = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        |"type": "string",
        |"minLength": 5,
        |"maxLength": 105,
        |"oneOf": [
        |  {
        |    "minLength": 10,
        |    "maxLength": 90
        |  }
        |]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"type": "string",
        |"minLength": 10,
        |"maxLength": 90
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }

  def strings2b = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        |"type": "string",
        |"minLength": 10,
        |"maxLength": 90,
        |"oneOf": [
        |  {
        |    "minLength": 5,
        |    "maxLength": 105
        |  }
        |]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"type": "string",
        |"minLength": 10,
        |"maxLength": 90
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }

  def strings3 = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        |"type": "string",
        |"oneOf": [
        |  {
        |    "pattern": "^[a-z0-9-]*$",
        |    "format": "uuid"
        |  },
        |  {
        |    "pattern": "^[a-z0-9-]*$",
        |    "format": "uuid"
        |  }
        |]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"type": "string",
        |"pattern": "^[a-z0-9-]*$",
        |"format": "uuid"
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }

  def strings4 = {
    val input = SpecHelpers.parseSchema(
      """
        |{
        |"type": "string",
        |"oneOf": [
        |  {
        |    "pattern": "^[a-z0-9-]*$",
        |    "format": "uuid"
        |  },
        |  {
        |    "pattern": "^[0-9\\.]*$",
        |    "format": "ipv4"
        |  }
        |]
        |}
      """.stripMargin)

    val expected = SpecHelpers.parseSchema(
      """
        |{
        |"type": "string"
        |}
      """.stripMargin)
    Mutate.weaken(input) must_== expected
  }

}
