package simplistic

import Conversions._

object Attributes {

  trait NamedAttribute {
    val name: String
  }

  /**
   * An attribute is a definition of a named value with a conversion to and from a string
   * representation.  It is useful when working with structures such as http request
   * parameters and simpledb queries.
   */
  case class Attribute[T](
    override val name: String,
    conversion: Conversion[T]
  ) extends NamedAttribute {

    /** When applied to a value, the conversion returns a name value pair of strings */
    def apply(value: T) = (name -> conversion(value))

    /**
     * When applied to a map of names to sets of string values, the conversion returns a
     * list of values retrieved from the set and converted from strings back to their
     * original type.
     */
    def apply(result: scala.collection.Map[String, Set[String]]): List[T] =
      if (! result.contains(name)) List.empty
      else (result(name) flatMap {
        case conversion(value) => List(value)
        case _ => List.empty
      }).toList
  }
  
  case class RequiredSingleValuedAttribute[T](
      override val name: String, 
      conversion: Conversion[T]
  ) extends NamedAttribute {
    def apply(value: T) = (name -> conversion(value))
    
    def apply(result: scala.collection.Map[String, Set[String]]): T = {
      result(name) head match {
        case conversion(value) => value
      }
    }
  }
  
  case class OptionalSingleValuedAttribute[T](
      override val name: String,
      conversion: Conversion[T]
  ) extends NamedAttribute {
    def apply(value: T) = (name -> conversion(value))
    
    def apply(result: scala.collection.Map[String, Set[String]]): Option[T] = {
      if (! result.contains(name)) None
      else result(name) head match {
        case conversion(value) => Some(value)
      }
    }
  }

  /** Create a simple attribute which performs no conversion on string values. */
  def attribute(name: String) = Attribute(name, Conversions.PassThrough)

  /** Create a typed attribute with an associated conversion to and from that type. */
  def attribute[T](name: String, conversion: Conversion[T]) = Attribute[T](name, conversion)
  

  /** Create an optional attribute that's not multi-valued */
  def singleAttribute[T](name: String) = OptionalSingleValuedAttribute(name, Conversions.PassThrough)
  def singleAttribute[T](name: String, conversion: Conversion[T]) = OptionalSingleValuedAttribute[T](name, conversion)
  
  /** Create a non-multi-valued attribute that's required */
  def singleRequiredAttribute[T](name: String) = RequiredSingleValuedAttribute(name, Conversions.PassThrough)  
  def singleRequiredAttribute[T](name: String, conversion: Conversion[T]) = RequiredSingleValuedAttribute[T](name, conversion)
}
