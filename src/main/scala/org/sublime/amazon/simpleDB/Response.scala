// Copyright 2008 Robin Barooah
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package org.sublime.amazon.simpleDB

import scala.xml._
import XMLFields._
import SimpleDBReader._

class Errors(xml: NodeSeq) {
  val error = new Error(node("Error", xml))
}

object Error {
  /** Extractor so we can pattern match errors **/
  def unapply(xml: NodeSeq): Option[(String, String, Option[Double])] = {
      val element = node("Errors", xml)
      if (element.length > 0) {
        val error = (new Errors(element)).error
        Some((error.code, error.message, error.boxUsage))
      } else None
  }
}

class Error(xml: NodeSeq) {
  val code = string("Code", xml)
  val message = string("Message", xml)
  val boxUsage = optionalDouble("BoxUsage", xml)
}

class SimpleDBResponse(xml: NodeSeq) {
  val metadata = readMetadata(xml)
}

class CreateDomainResponse(xml: NodeSeq) extends SimpleDBResponse(xml)

class DeleteDomainResponse(xml: NodeSeq) extends SimpleDBResponse(xml)

class ListDomainsResponse(xml: NodeSeq) extends SimpleDBResponse(xml) {
  val result = new ListDomainsResult(node("ListDomainsResult", xml))
}

class DomainMetadataResponse(xml: NodeSeq) extends SimpleDBResponse(xml) {
  val result = new DomainMetadataResult(node("DomainMetadataResult", xml))
}

class PutAttributesResponse(xml: NodeSeq) extends SimpleDBResponse(xml)

class BatchPutAttributesResponse(xml: NodeSeq) extends SimpleDBResponse(xml)

class DeleteAttributesResponse(xml: NodeSeq) extends SimpleDBResponse(xml)

class GetAttributesResponse(xml: NodeSeq) extends SimpleDBResponse(xml) {
  val result = new GetAttributesResult(node("GetAttributesResult", xml))
}

class QueryResponse(xml: NodeSeq) extends SimpleDBResponse(xml) {
  val result = new QueryResult(node("QueryResult", xml))
}

class QueryWithAttributesResponse(xml: NodeSeq) extends SimpleDBResponse(xml) {
  val result = new QueryWithAttributesResult(node("QueryWithAttributesResult", xml))
}

abstract class ItemWithAttributesResult(xml: NodeSeq) {
  class Item(xml: NodeSeq) {
    import Format._
    val name = string("Name", xml)
    val attributes = readAttributes(xml)
    override def toString = name + "\n" + ("-" * name.length) + "\n" + formatAttributes(attributes)
  }

  val nextToken = optionalString("NextToken", xml)
  val items = nodes("Item", xml) map (new Item(_))
  override def toString = items mkString "\n\n"
}

class QueryResult(xml: NodeSeq) {
  val itemNames = strings("ItemName", xml)
  val nextToken = optionalString("NextToken", xml)
  override def toString = itemNames mkString ", "
}

class QueryWithAttributesResult(xml: NodeSeq) extends ItemWithAttributesResult(xml)

class SelectResult(xml: NodeSeq) extends ItemWithAttributesResult(xml)

class SelectResponse(xml: NodeSeq) extends SimpleDBResponse(xml) {
  val result = new QueryWithAttributesResult(node("SelectResult", xml))
}

class GetAttributesResult(xml: NodeSeq) {
  import Format._
  val attributes = readAttributes(xml)
  override def toString = formatAttributes(attributes)
}

class ListDomainsResult(xml: NodeSeq) {
  val domainNames = strings("DomainName", xml)
  val nextToken = optionalString("NextToken", xml)
  override def toString = domainNames mkString ("\n")
}

class DomainMetadataResult(xml: NodeSeq) {

  // oddly this field is listed in the documentation
  // but isn't in the real responses
  // val creation = dateField("CreationDateTime")

  val itemCount = int("ItemCount", xml)
  val itemNameSizeBytes = int("ItemNamesSizeBytes", xml)
  val attributeNameCount = int("AttributeNameCount", xml)
  val attributeNameSizeBytes = int("AttributeNamesSizeBytes", xml)
  val attributeValueCount = int("AttributeValueCount", xml)
  val attributeValueSizeBytes = int("AttributeValuesSizeBytes", xml)
  val timestamp = int("Timestamp", xml)

  override def toString = List(
    //"created: " + creation,
    "items: " + itemCount,
    "item names in bytes: " + itemNameSizeBytes,
    "attibute names: " + attributeNameCount,
    "attibute names in bytes: " + attributeNameSizeBytes,
    "attribute value count: " + attributeValueCount,
    "attribute value size in bytes: " + attributeValueSizeBytes,
    "timestamp: " + timestamp
  ) mkString ("\n")
}

class ResponseMetadata(xml: NodeSeq) {
  val requestId = string("RequestId", xml)
  val boxUsage = double("BoxUsage", xml)
  override def toString = "Box Usage: " + boxUsage + "s" + " request id: " + requestId
}

/**
 * Functions for decomposing simpleDB specific types.
 */
object SimpleDBReader {
  def readMetadata(xml: NodeSeq) = new ResponseMetadata(node("ResponseMetadata", xml))

  def readAttributes(xml: NodeSeq): Map[String, Set[String]] = {
    var found = Map[String,Set[String]]()

    def add(name: String, value: String) {
      found = found updated (name, (found getOrElse(name, Set())) + value)
    }

    for (node <- nodes("Attribute", xml))
      add(string("Name", node), string("Value", node))

    found
  }
}

object Format {
  def formatAttributes(map:Map[String, Set[String]]) =
    (map.keys map ( n => n + ": " + (map(n) mkString ", "))) mkString "\n"
}

/**
 * functions for breaking down XML
 */
object XMLFields {
  def node(name: String, xml: NodeSeq) =(xml \ name)
  def nodes(name: String, xml: NodeSeq) = (xml \ name)
  def string(name: String, xml: NodeSeq) = node(name, xml) text
  def optionalString(name: String, xml: NodeSeq) :Option[String] = {
    val found = string(name, xml)
    if (found.length > 0) Some(found)
    else None
  }
  def strings(name: String, xml: NodeSeq) = nodes(name, xml) map (_.text)
  def dateField(name: String, xml: NodeSeq) = dateFormat.parse(string(name, xml))
  def int(name: String, xml: NodeSeq) = Integer.parseInt(string(name, xml))
  def double(name: String, xml: NodeSeq) = java.lang.Double.parseDouble(string(name, xml))
  def optionalDouble(name: String, xml: NodeSeq): Option[Double] = {
    val found = string(name, xml)
    if (found.length > 0) Some(java.lang.Double.parseDouble(found))
    else None
  }
  def boolean(name: String, xml: NodeSeq) = string(name, xml) match {
    case "True" => true
    case "False" => false
  }
  def dateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
}
