//> using scala 3.5
//> using dep "org.postgresql:postgresql:42.7.4"

import scala.language.experimental.namedTuples

import Config.given

@main
def main =
  val res = Database.query("""
      SELECT *
      FROM purchases
      NATURAL JOIN customers
      WHERE product_id = 11
      AND purchase_date > '2024-07-15'
    """)
  println("results:")
  for row <- res do
    println(s"${row.purchase_date}: ${row.name}\t(${row.email})")
