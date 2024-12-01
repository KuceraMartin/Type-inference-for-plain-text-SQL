import java.sql.*

import scala.language.experimental.namedTuples

import scala.quoted.*
import scala.sys.process.*
import scala.NamedTuple.*

object Database:

  transparent inline def query(inline query: String)(using connection: Connection): Iterator[?] =
    ${ queryImpl('query, 'connection) }
  

  private def queryImpl(queryExpr: Expr[String], connection: Expr[Connection])(using Quotes): Expr[Iterator[?]] =
    // 1. get the query as a value
    val query = queryExpr.valueOrAbort

    // 2. dry-run the query to get the column names and types
    val columns = dryRun(query)

    // 3. construct the result type
    val resultType = constructNamedTupleTypeRepr(columns)

    // 4. construct the conversion ResultSet => Tuple
    def resultSetToTuple(resultSet: Expr[ResultSet]): Expr[Tuple] =
      val values = columns.map: column =>
        val columnNameExpr = Expr(column.name)
        '{ $resultSet.getObject($columnNameExpr) }
      Expr.ofTupleFromSeq(values)

    // 5. construct the body of the `query` function
    resultType.asType match
      case '[t] => '{
        val statement = $connection.createStatement()
        val resultSet = statement.executeQuery($queryExpr)
        def makeRow(rs: ResultSet) = ${ resultSetToTuple('rs) }.asInstanceOf[t]
        ResultIterator(resultSet, makeRow)
      }
  end queryImpl


  private def dryRun(query: String): Seq[Column] =
    val dryRunQuery = s"""
        CREATE TEMP VIEW temp_view AS
          SELECT *
          FROM ($query) q
          LIMIT 0;

        SELECT column_name, data_type
        FROM information_schema.columns
        WHERE table_name = 'temp_view';

        DROP VIEW temp_view;
      """
    
    // Running the query in a hacky way - starting an external process and running psql.
    // Ideally, we would use java.sql just like the runtime app but this is currently not
    // possible due to a bug in Scala: https://github.com/scala/scala3/issues/20560
    Process(s"psql -d '${ Config.dbUri }' -c \"$dryRunQuery\"").!!
      .split('\n')
      .drop(3)
      .flatMap: row =>
        row.split('|').map(_.trim).toSeq match
          case Seq(name, dbType) =>
            Some((name, dbType))
          case _ => None
  end dryRun


  /**
   * Constructs a named tuple type from a list of columns. A named tuple consists
   * of two unnamed tulpes: the names as literal constant string types, and the
   * element types. We construct the two tuples separately and then create the
   * named tuple type from them,
   */
  private def constructNamedTupleTypeRepr(columns: Seq[Column])(using Quotes) =
    import quotes.reflect.*

    // Helper type lambda to convince the type system that T represents a tuple.
    type TupleSubtype[T <: Tuple] = T

    // the first tuple of names
    val names = columns.foldRight(TypeRepr.of[EmptyTuple]): (column, acc) =>
      val name = ConstantType(StringConstant(column.name))
      (name.asType, acc.asType) match
        case ('[n], '[TupleSubtype[ns]]) => TypeRepr.of[n *: ns]

    // the second tuple of element types
    val types = columns.foldRight(TypeRepr.of[EmptyTuple]): (column, acc) =>
      val tpe = dbTypeToTypeRepr(column.dbType)
      (tpe.asType, acc.asType) match
        case ('[t], '[TupleSubtype[ts]]) => TypeRepr.of[t *: ts]

    // using the two tuple types to create a named tuple type
    (names.asType, types.asType) match
      case ('[TupleSubtype[ns]], '[TupleSubtype[ts]]) => TypeRepr.of[NamedTuple[ns, ts]]
  end constructNamedTupleTypeRepr


  private def dbTypeToTypeRepr(dbType: String)(using Quotes) =
    import quotes.reflect.*

    dbType match
      case "text" | "character varying" => TypeRepr.of[String]
      case "integer" => TypeRepr.of[Int]
      case "timestamp without time zone" => TypeRepr.of[java.sql.Timestamp]
      case _ => throw new Exception(s"Uknown database type $dbType.")
  end dbTypeToTypeRepr


  private type Column = (name: String, dbType: String)
