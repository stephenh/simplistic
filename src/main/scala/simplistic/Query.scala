package simplistic


import Attributes._
import Quoting._

object Query {

  abstract class Expression {
    //def query : String
  }

  trait Combinable extends Expression {
    def intersection(other: Combinable) = Combination("intersection", this, other)
    def union(other: Combinable) = Combination("union", this, other)
  }

  case class Combination(operation: String, lhs: Expression, rhs: Expression) extends Expression with Combinable {
    override def toString = lhs + " " + operation + " " + rhs
    def sort[T](attribute: Attribute[T]) = SortedCombination(this, attribute.name)
  }

  case class SortedCombination(target: Combination, name: String) extends Expression {
    override def toString = target + " sort " + quoteValue(name) + " asc"
    def desc = DescendingSortedCombination(target, name)
  }

  case class DescendingSortedCombination(target: Combination, name: String) extends Expression {
      override def toString = target + " sort "+quoteValue(name) + " desc"
  }

  trait Negatable extends Expression

  case class Negation(target: Negatable) extends Expression with Combinable {
    override def toString = "not " + target;
  }

  case class DescendingSort(target: Sortable, name: String) extends Expression with Negatable {
    //TODO: name quoting    
    override def toString = target.component + " order by " + name + " desc"
  }

  case class AscendingSort(target: Sortable, name: String) extends Expression with Negatable {
    def desc = DescendingSort(target, name)
    //TODO: name quoting
    override def toString = target.component + " order by " + name + " asc"
  }

  trait Sortable extends Predicate {
    def sort [T](attribute: Attribute[T]) : AscendingSort = AscendingSort(this, attribute.name)
  }

  abstract class Predicate extends Expression with Negatable {
    def and(other: Predicate) = Conjunction("and", this, other)
    def or(other: Predicate) = Conjunction("and", this, other)
    def intersection(other: Predicate) = Combination("intersection", this, other)
    def union(other: Predicate) = Combination("union", this, other)
    def unary_! = Negation(this)

    def component : String
    override def toString = component
  }

  case class Conjunction(operator: String, lhs: Predicate, rhs: Predicate) extends Predicate with Sortable {
    def component = lhs.component + " " + operator + " " + rhs.component
  }

  case class Comparison [T](operator: String, attribute: Attribute[T], value: T) extends Predicate with Sortable {
    def component = {
      val (name, converted) = attribute(value)
      // TODO: name should really be quoteName() but fakeSDB doesn't support this...
      name + " " + operator + " " + quoteValue(converted)
    }
  }

  implicit def toQueryAttribute[T](a: Attribute[T]) : QueryAttribute [T] = new QueryAttribute(a)

  class QueryAttribute [T](a: Attribute[T]) {
    private def comparison(op: String, value: T) = Comparison(op, a, value)

    def is(value: T) = comparison("=", value)
    def is_not(value: T) = comparison("!=", value)
    def >(value: T) = comparison(">", value)
    def <(value: T) = comparison("<", value)
    def >=(value: T) = comparison(">=", value)
    def <=(value: T) = comparison("<=", value)
    def starts_with(value: T) = comparison("starts-with", value)
    def does_not_start_with(value: T) = comparison("does_not_start_with", value)
  }

  implicit def toQueryableDomain(d: Domain) : QueryableDomain = new QueryableDomain(d)

  class QueryableDomain(d: Domain) {
    /*** EXPERIMENTAL METHODS ASSOCIATED WITH THE QUERY DSL ***/
    private def attributeSet(attrs: NamedAttribute*) : Set[String] =
      (Set[String]() /: (for (a <- attrs) yield (Set[String](a.name)))) (_ ++ _)

    def apply(expr: Expression) = d.withAttributes(expr.toString)
    def apply(attrs: NamedAttribute*)(expr: Expression) =
      d.withAttributes(expr.toString, attributeSet(attrs: _*))
    def findFirst(expression: Expression): Option[ItemSnapshot] = apply(expression).headOption
  }
}
