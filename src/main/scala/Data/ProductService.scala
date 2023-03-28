package Data

trait ProductService:
  type BrandName = String
  type ProductName = String

  def getPrice(product: ProductName, brand: BrandName): Double
  def getDefaultBrand(product: ProductName): BrandName

class ProductImpl extends ProductService:
  // TODO - Part 2 Step 2
  private val BEER = "beer"
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
    default_products.get(product)
end ProductImpl
