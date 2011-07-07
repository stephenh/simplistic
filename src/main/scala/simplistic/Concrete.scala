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

package simplistic

import Request._
import scala.xml._

trait Concrete {
  def doRequest(request: SimpleDBRequest): Elem

  def awsAccessKeyId: String

  def now(): String = dateFormat.format(new java.util.Date)

  def dateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ")

  trait Basics {
    def timeStamp = now()
    def awsAccessKeyId = Concrete.this.awsAccessKeyId
  }

  class ListDomainsRequest(
      val nextToken: Option[String],
      val maxNumberOfDomains: Option[Int]
  ) extends ListDomains with Basics {
    def response = new ListDomainsResponse(doRequest(this))
  }

  object ListDomainsRequest {
    def start = new ListDomainsRequest(None, Some(2))
    def start(maxNumberOfDomains: Int) = new ListDomainsRequest(None, Some(maxNumberOfDomains))
    def next (response: ListDomainsResponse): Option[ListDomainsRequest] =
      response.result.nextToken map { token => new ListDomainsRequest(Some(token), Some(2)) }
  }

  class CreateDomainRequest (val domainName: String) extends CreateDomain with Basics {
    def response = new CreateDomainResponse(doRequest(this))
  }

  class DeleteDomainRequest (val domainName: String) extends DeleteDomain with Basics
  {
    def response = new DeleteDomainResponse(doRequest(this))
  }

  class DomainMetadataRequest(val domainName: String) extends DomainMetadata with Basics {
    def response = new DomainMetadataResponse(doRequest(this))
  }

  class PutAttributesRequest(
    val domainName: String,
    val itemName: String,
    val attributes: Map[String, (Set[String], Boolean)],
    val condition: PutConditions.PutCondition = PutConditions.NoCondition
  ) extends PutAttributes with Basics {
    def response = new PutAttributesResponse(doRequest(this))
  }

  class BatchPutAttributesRequest(
    val domainName: String,
    val operations: List[AttributeOperation]
  ) extends BatchPutAttributes with Basics {
    def response = new BatchPutAttributesResponse(doRequest(this))
  }

  class DeleteAttributesRequest(
    val domainName: String,
    val itemName: String,
    val attributes: Map[String, Set[String]]
  ) extends DeleteAttributes with Basics {
    def response = new DeleteAttributesResponse(doRequest(this))
  }

  class GetAttributesRequest(
    val domainName: String,
    val itemName: String,
    val attributes: Set[String],
    val consistency: Consistency
  ) extends GetAttributes with Basics {
    def response = new GetAttributesResponse(doRequest(this))
  }

  class SelectRequest(
    val selectExpression: String,
    val consistency: Consistency,
    val nextToken: Option[String],
    val maxNumberOfItems: Option[Int]
  ) extends Select with Basics {
    def response = new SelectResponse(doRequest(this))
  }

  object SelectRequest {
    def start(selectExpression: String, consistency: Consistency) =
      new SelectRequest(selectExpression, consistency, None, None)

    def next(req: SelectRequest, res: SelectResponse): Option[SelectRequest] =
      res.result.nextToken map { token => new SelectRequest(req.selectExpression, req.consistency, Some(token), None) }
  }
}
