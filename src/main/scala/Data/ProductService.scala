package Data

trait ProductService:
  type BrandName = String
  type ProductName = String

  def getPrice(product: ProductName, brand: BrandName): Double
  def getDefaultBrand(product: ProductName): BrandName
  def toString(
      name: ProductName,
      brand: Option[BrandName],
      quantity: Int
  ): String

class ProductImpl extends ProductService:
  // TODO - Part 2 Step 2
  private val BEER = "biere"
  private val CROISSANT = "croissant"

  private val default_products = Map(
    BEER -> "boxer",
    CROISSANT -> "maison"
  )

  private val products = Map(
    BEER -> Map(
      "boxer" -> 1.0,
      "farmer" -> 1.0,
      "wittek" -> 2.0,
      "punkipa" -> 3.0,
      "jackhammer" -> 3.0,
      "tenebreuse" -> 4.0
    ),
    CROISSANT -> Map("maison" -> 2.0, "cailler" -> 2.0)
  )

  def getPrice(product: ProductName, brand: String): Double =
    products.get(product).get(brand)
  def getDefaultBrand(product: ProductName): BrandName =
    default_products(product)

  def toString(
      name: ProductName,
      brand: Option[BrandName],
      quantity: Int
  ): String =
    val b = brand.getOrElse(getDefaultBrand(name))
    s"${quantity} ${if name == BEER then b else s"${name} ${b}"}"
end ProductImpl
