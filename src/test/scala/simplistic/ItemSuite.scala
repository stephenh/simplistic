package simplistic

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class ItemSuite extends WordSpec with ShouldMatchers with TestUtil.CleanBefore with TestUtil.StopAndStartServer {
  import TestUtil._

  val testDomain = account.domain("test")

  override def beforeEach() {
    super.beforeEach()
    testDomain.create()
  }

  "Item" should {

    "return existing attribute if it exists" in {
      val item = testDomain.item("test1")
      item += (("foo", "bar"))

      item.attribute("foo") should be === Set("bar")
    }

    "return multiple existing attributes" in {
      val item = testDomain.item("test2")
      item += ("foo", Set("bar", "baz"))

      item.attribute("foo") should be === Set("bar", "baz")
    }

    "return empty set if attribute doesn't exist" in {
      val item = testDomain.item("test3")
      item += (("foo", "bar"))

      item.attribute("non-existing") should be === Set.empty
    }

    "return only the requested attributes" in {
      val item = testDomain.item("test4")
      item += (("a1", "foo"))
      item += (("a2", "bar"))
      item += (("a3", "baz"))

      item.attributes(Set("a1", "a3")).self should be === Map(
        "a1" -> Set("foo"),
        "a3" -> Set("baz")
      )
    }

    "return attribute using typed attributes" in {
      val item = testDomain.item("test5")
      val attr = Attributes.attribute("foo")
      item += attr("bar")

      item.attribute(attr) should be === Set("bar")
    }

    "provide an Option of all its attributes" in {
      val item = testDomain.item("test6")
      item += (("foo", "bar"))
      item.attributesOption should be === Some(Map("foo" -> Set("bar")))
    }

    "delete itself (all of its attributes)" in {
      val item = testDomain.item("test7")
      item += (("foo", "bar"))
      item.delete()
      item.attributesOption should be === None
    }

    "delete one of its attributes" in {
      val item = testDomain.item("test8")
      item += (("foo", "bar"))
      item += (("quux", "oteq"))
      item -= "foo"
      item.attributes should be === Map("quux" -> Set("oteq"))
    }

    "delete all values of one of its attributes" in {
      val item = testDomain.item("test9")
      item += ("foo", Set("bar", "baz"))
      item -= "foo"
      item.attributes should be === Map()
    }

    "delete an attribute using typed attribute" in {
      val item = testDomain.item("test10")
      val attr = Attributes.attribute("foo")
      item += attr("bar")
      item -= attr
      item.attributesOption should be === None
    }
  }
}