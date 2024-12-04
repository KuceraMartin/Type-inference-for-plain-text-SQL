import java.sql.*

/**
 * A wrapper over java.sql.ResultsSet that transforms it into an Iterator[R]
 * @param resultSet The result of an SQL query. It has three important methods:
 *     hasNext: Boolean                        whether there are any more rows available
 *     next(): Boolean                         side-effecting method, moves the cursor to the next row
 *     getObject(columnName: String): Object   get the value of columnName at the current row
 * @param conversion A lambda function that is used to convert a row to an instance of R.
 *     It will use resultSet.getObject(columnName) to do that.
 */
class ResultIterator[R](resultSet: ResultSet, conversion: ResultSet => R) extends Iterator[R]:

  private var hasNextRes: Option[Boolean] = None

  def hasNext: Boolean =
    hasNextRes match
      case Some(res) => res
      case None =>
        hasNextRes = Some(resultSet.next())
        hasNextRes.get
    
  def next(): R =
    require(hasNext)
    hasNextRes = None
    conversion(resultSet)

end ResultIterator
