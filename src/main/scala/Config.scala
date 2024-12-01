import java.sql.*

object Config:
  val dbUri = "postgresql://localhost:5432/eshop?user=user&password=1234"
  given dbConnection: Connection = DriverManager.getConnection(s"jdbc:$dbUri")
