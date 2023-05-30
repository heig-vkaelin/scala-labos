package Data

import scala.collection.mutable

trait AccountService:
  /** Retrieve the balance of a given account
    * @param user
    *   the name of the user whose account will be retrieve
    * @return
    *   the current balance of the user
    * @throws NoSuchElementException
    *   if the account does not exist
    */
  def getAccountBalance(user: String): Double

  /** Add an account to the existing accounts
    * @param user
    *   the name of the user
    * @param balance
    *   the initial balance value
    * @throws IllegalArgumentException
    *   if the account already exists
    */
  def addAccount(user: String, balance: Double = 30): Unit

  /** Indicate is an account exist
    * @param user
    *   the name of the user whose account is checked to exist
    * @return
    *   whether the account exists or not
    */
  def isAccountExisting(user: String): Boolean

  /** Update an account by decreasing its balance.
    * @param user
    *   the name of the user whose account will be updated
    * @param amount
    *   the amount to decrease
    * @return
    *   the new balance
    * @throws NoSuchElementException
    *   if the account does not exist
    * @throws Exception
    *   if the account does not have enough money
    */
  def purchase(user: String, amount: Double): Double

class AccountImpl extends AccountService:
  private val accounts = mutable.Map[String, Double]()

  def getAccountBalance(user: String): Double =
    accounts(user)
  def addAccount(user: String, balance: Double): Unit =
    if isAccountExisting(user) then
      throw new IllegalArgumentException("Account already exists")
    else accounts.put(user, balance)
  def isAccountExisting(user: String): Boolean =
    accounts.contains(user)
  def purchase(user: String, amount: Double): Double =
    accounts
      .updateWith(user)(current =>
        if current.get >= amount then Some(current.get - amount)
        else throw new Exception("Not enough money")
      )
      .get
end AccountImpl
