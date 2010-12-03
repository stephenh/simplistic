package simplistic

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class AttributesSuite extends WordSpec with ShouldMatchers {
  import Attributes._
  
  "RequiredSingleValuedAttribute" should {
    
    "error when missing" in {
      evaluating { attribute("a")(Map[String, Set[String]]()) } should produce [MissingRequiredAttribute]
    }
  }
}