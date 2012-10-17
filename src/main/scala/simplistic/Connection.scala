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

import java.io.IOException

import org.apache.http.HttpResponse
import org.apache.http.client.HttpClient;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.impl.conn.tsccm.ThreadSafeClientConnManager;
import org.apache.http.params._
import org.apache.http.client.methods.HttpPost

import scala.xml._

class ClientConfiguration {
  val maxConnections: Int = 50
  val socketTimeout: Int = 50 * 1000
  val connectionTimeout: Int = 50 * 1000
  val maxErrorRetry: Int = 10
  val socketLinger: Int = -1
}

object HttpClientFactory {
  def createHttpClient(config: ClientConfiguration): HttpClient = {
    val clientParams = new BasicHttpParams
    HttpConnectionParams.setConnectionTimeout(clientParams, config.connectionTimeout)
    HttpConnectionParams.setSoTimeout(clientParams, config.socketTimeout)
    HttpConnectionParams.setStaleCheckingEnabled(clientParams, true)
    HttpConnectionParams.setTcpNoDelay(clientParams, true)

    if (config.socketLinger >= 0) {
      HttpConnectionParams.setLinger(clientParams, config.socketLinger)
    }

    val connectionManager = new ThreadSafeClientConnManager
    connectionManager.setDefaultMaxPerRoute(config.maxConnections)
    connectionManager.setMaxTotal(config.maxConnections)

    new DefaultHttpClient(connectionManager, clientParams)
  }
}

class Connection(val awsAccessKeyId: String, awsSecretKey: String, val url: String, val config: ClientConfiguration = new ClientConfiguration) {
  import Exceptions.toException

  private val signer = new Signer(awsSecretKey)

  @transient var trace = false

  val client = HttpClientFactory.createHttpClient(config)

  def makeRequest(request: SimpleDBRequest): Elem = {
    if (trace) diagnose(request.parameters)

    retryIfUnavailable {
      val signedUrl = url + QueryParameters(signer.sign(request.parameters))
      if (trace) diagnose(signedUrl)
      val method = new HttpPost(signedUrl)
      var response: HttpResponse = null

      try {
        response = client.execute(method)
        val xml = XML.load(response.getEntity.getContent)
        if (trace) diagnose(xml)
        xml match {
          case Error(code, message, boxUsage) => throw toException(code, message, boxUsage)
          case _ => xml
        }
      } finally {
        try {
          method.getEntity.getContent.close
        } catch {
          case _: Exception => // ignore
        }
      }
    }
  }

  private def retryIfUnavailable[T](f: => T): T = {
    import Exceptions.ServiceUnavailable

    var retriesLeft = config.maxErrorRetry
    var delay = 50 // milliseconds
    while (true) {
      try {
        return f
      } catch {
        // note: IOException subsumes java.net.SocketException, org.apache.http.NoHttpResponseException
        // (and others presumably)
        case e @ (_: ServiceUnavailable | _: IOException) =>
          if (retriesLeft > 0) {
            retriesLeft -= 1
            delay *= 2 // exponential backoff
            Thread.sleep(delay)
          } else {
            throw e
          }
      }
    }
    throw new IllegalStateException("unreachable")
  }

  def printer = new PrettyPrinter(80, 2)

  def diagnose(s: String) {
    Console.println(s)
  }

  def diagnose(xml: Node) {
    Console.println(printer.format(xml))
  }

  def diagnose(parameters: Map[String, String]) {
    Console.println(parameters.keys map (k => k + ": " + parameters(k)) mkString "\n")
  }
}
