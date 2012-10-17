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

/** Class to sign SimpleDB REST HTTP requests. */
class Signer(key: String) {
  import scala.util.Sorting._
  import javax.crypto.spec.SecretKeySpec

  private val awsSecretKey = new SecretKeySpec(key.getBytes, "hmacsha1")

  /** Given a map of request parameters, return a signed map containing
   *  the same parameters.  This is idempotent and will return a correct
   *  value map even if what is passed in is already signed.
   */
  def sign(m: Map[String, String]) = {
    val versioned = m updated ("SignatureVersion", "1")
    versioned + signature(versioned)
  }

  /** Return the signature of a map of request parameters as a tuple. */
  def signature (m:Map[String, String]) :(String,String) = {
    val keysToInclude = m.keys filterNot (_ equals "Signature")

    val sortedKeys = stableSort(keysToInclude.toList, (_: String).compareToIgnoreCase(_: String) < 0)

    val combinedParameters = sortedKeys map { param => param + m(param) } mkString

    val digest = {
      val mac = javax.crypto.Mac.getInstance("hmacsha1")
      mac.init(awsSecretKey)
      mac.update(combinedParameters.getBytes)
      mac.doFinal
    }

    val base64Encoder = new org.apache.commons.codec.binary.Base64
    val encode = base64Encoder.encode(digest)

    "Signature" -> new String(encode).trim()
  }
}
