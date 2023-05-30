package Data

import ProductService.*
import scala.concurrent.duration.*
import scala.concurrent.Future
import Utils.FutureOps.randomSchedule

trait ProductService:
  def getPrice(product: ProductName, brand: BrandName): Double
  def getDefaultBrand(product: ProductName): BrandName
  def toString(
      name: ProductName,
      brand: Option[BrandName],
      quantity: Int
  ): String
  def prepare(product: ProductName, brand: BrandName): Future[Unit]

object ProductService:
  type BrandName = String
  type ProductName = String

  case class Delivery(mean: Duration, std: Duration, successRate: Double)

class ProductImpl extends ProductService:
  // Products available
  private val BEER = "biere"
  private val CROISSANT = "croissant"

  private val default_products = Map(
    BEER -> "boxer",
    CROISSANT -> "maison"
  )

  private val products = Map(
    BEER -> Map(
      "boxer" -> (1.0, Delivery(1.second, 0.2.second, 0.9)),
      "farmer" -> (1.0, Delivery(0.5.second, 0.2.second, 0.7)),
      "wittekop" -> (2.0, Delivery(1.5.second, 0.2.second, 0.9)),
      "punkipa" -> (3.0, Delivery(3.second, 0.2.second, 0.8)),
      "jackhammer" -> (3.0, Delivery(2.5.second, 0.2.second, 0.3)),
      "tenebreuse" -> (4.0, Delivery(2.second, 0.2.second, 0.1))
    ),
    CROISSANT -> Map(
      "maison" -> (2.0, Delivery(2.second, 0.2.second, 0.9)),
      "cailler" -> (2.0, Delivery(3.5.second, 0.2.second, 0.7))
    )
  )

  /** Return the price of a given product.
    * @param product
    *   : the product name
    * @param brand
    *   : the brand name
    * @return
    *   the price of the product
    */
  def getPrice(product: ProductName, brand: String): Double =
    products(product)(brand)._1

  /** Return the default brand for a given product.
    * @param product
    *   : the product name
    * @return
    */
  def getDefaultBrand(product: ProductName): BrandName =
    default_products(product)

    /** Return a string representation of a product.
      * @param name
      *   : the product name
      * @param brand
      *   : the optional brand name
      * @param quantity
      *   : the quantity of the product
      * @return
      */
  def toString(
      name: ProductName,
      brand: Option[BrandName],
      quantity: Int
  ): String =
    val b = brand.getOrElse(getDefaultBrand(name))
    s"${quantity} ${if name == BEER then b else s"${name} ${b}"}"

    /** Prepare a product for delivery.
      *
      * @param product
      *   : the product name
      * @param brand
      *   : the brand name
      * @return
      *   a future that will be completed when the product is ready
      */
  def prepare(product: ProductName, brand: BrandName): Future[Unit] =
    val (_, delivery) = products(product)(brand)
    println(s"Preparing ${product} ${brand}")
    randomSchedule(delivery.mean, delivery.std, delivery.successRate)
end ProductImpl
