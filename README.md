# Type inference for plain-text SQL

This is a companion repository to my [Functional Scala Conference talk](https://www.functionalscala.com/authors-and-speakers/martin-kucera). The slides can be found here: <https://docs.google.com/presentation/d/12EkZQKwi4cETRIsI5y0if1mG6GRuRxkeI8p7iMUBivc/edit?usp=sharing>

There is a dummy database that you can start by running `docker-compose up`. Then you can open the root directory in your favorite editor, start a Metals project, go to `src/main/scala/main.scala`, edit the SQL query and watch the inferred types of `res` and `row` variables change.

Feel free to contact me if you have any questions or feedback!
