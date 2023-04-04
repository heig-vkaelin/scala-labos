package Chat

import Data.ProductService.{ProductName, BrandName}

/** This sealed trait represents a node of the tree.
  */
sealed trait ExprTree

/** Declarations of the nodes' types.
  */
object ExprTree:
  // TODO - Part 2 Step 3
  // Example cases
  case object Thirsty extends ExprTree
  case object Hungry extends ExprTree

  // Operators
  case class And(left: ExprTree, right: ExprTree) extends ExprTree
  case class Or(left: ExprTree, right: ExprTree) extends ExprTree

  // Request types
  case class Identification(username: String) extends ExprTree
  case class Command(expr: ExprTree) extends ExprTree
  case object Balance extends ExprTree
  case class Price(expr: ExprTree) extends ExprTree

  // Product
  case class Product(name: ProductName, brand: Option[BrandName], quantity: Int)
      extends ExprTree
