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

object SimpleDBAccount {
  val defaultURL = "https://sdb.amazonaws.com"
}

/**
 * A simple concrete implementation of the SimpleDB API.  Instantiate one of these with
 * your AWS credentials and then use the methods defined in the SimpleAPI trait.
 */
class SimpleDBAccount(
  val awsAccessKeyId: String,
  awsSecretKey: String,
  val url: String = SimpleDBAccount.defaultURL,
  val connectionPool: ConnectionPool = new DefaultConnectionPool
) extends SimpleAPI {

  val connection = new Connection(awsAccessKeyId, awsSecretKey, url, connectionPool)

  def doRequest(req: SimpleDBRequest): Elem = connection.makeRequest(req)
}
