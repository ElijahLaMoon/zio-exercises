package net.degoes.zio

import zio._
import scala.collection.immutable.Nil
import scala.annotation.tailrec

object Looping extends App {
  import zio.console._

  /**
   * EXERCISE
   *
   * Implement a `repeat` combinator using `flatMap` (or `zipRight`) and recursion.
   */
  def repeat[R, E, A](n: Int)(effect: ZIO[R, E, A]): ZIO[R, E, Chunk[A]] =
    n match {
      case _ if n <= 0 => effect.map(Chunk(_))
      case _           => effect *>   repeat(n - 1)(effect)
    }

  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    repeat(3)(putStrLn("All work and no play makes Jack a dull boy")).exitCode
}

object Interview extends App {
  import java.io.IOException
  import zio.console._

  private val questions =
    "Where were you born?"            ::
      "What color are your eyes?"     ::
      "What is your favorite movie?"  ::
      "What is your favorite number?" :: Nil

  /**
   * EXERCISE
   *
   * Implement the `getAllAnswers` function in such a fashion that it will ask
   * the user each question and collect them all into a list.
   */
  def getAllAnswers(questions: List[String]): ZIO[Console, IOException, List[String]] =
    questions match {
      case Nil     => ZIO.succeed(List())
      case q :: qs =>
        for {
          _  <- putStrLn(q)
          a  <- getStrLn
          as <- getAllAnswers(qs)
        } yield a :: as
    }

  /**
   * EXERCISE
   *
   * Use the preceding `getAllAnswers` function, together with the predefined
   * `questions`, to ask the user a bunch of questions, and print the answers.
   */
  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    getAllAnswers(questions).flatMap {
      case a1 :: a2 :: a3 :: a4 :: Nil => putStrLn(
        s"""
         |You were born in $a1, you have ${a2.toLowerCase} eyes,
         |your favorite movie is $a3, and your favorite number is $a4
         """.stripMargin)
      case _                           => putStrLn("")
    }.exitCode
}

object InterviewGeneric extends App {
  import zio.console._

  private val questions =
    "Where where you born?"           ::
      "What color are your eyes?"     ::
      "What is your favorite movie?"  ::
      "What is your favorite number?" :: Nil

  /**
   * EXERCISE
   *
   * Implement the `iterateAndCollect` function.
   */
  def iterateAndCollect[R, E, A, B](as: List[A])(f: A => ZIO[R, E, B]): ZIO[R, E, List[B]] =
    as match {
      case Nil     => ZIO.succeed(List())
      case a :: as => f(a).flatMap(_ => iterateAndCollect(as)(f))
    }

  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = {
    iterateAndCollect(questions)(putStrLn(_)).exitCode
  }
}

object InterviewForeach extends App {
  import zio.console._

  private val questions =
    "Where were you born?"            ::
      "What color are your eyes?"     ::
      "What is your favorite movie?"  ::
      "What is your favorite number?" :: Nil

  /**
   * EXERCISE
   *
   * Using `ZIO.foreach`, iterate over each question in `questions`, print the
   * question to the user (`putStrLn`), read the answer from the user
   * (`getStrLn`), and collect all answers into a collection. Finally, print
   * out the contents of the collection.
   */
  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    ZIO
      .foreach(questions){ q => putStrLn(q) *> getStrLn }
      .flatMap {
        case a1 :: a2 :: a3 :: a4 :: Nil =>
          putStrLn(
            s"""
               |Answer 1: $a1
               |Answer 2: $a2
               |Answer 3: $a3
               |Answer 4: $a4
           """.stripMargin
          )
        case _ => putStrLn("")
      }
      .exitCode
}

object WhileLoop extends App {
  import zio.console._

  /**
   * EXERCISE
   *
   * Implement the functional effect version of a while loop.
   */
  def whileLoop[R, E, A](cond: UIO[Boolean])(zio: ZIO[R, E, A]): ZIO[R, E, Chunk[A]] =
    cond flatMap {
      case true => zio *> whileLoop(cond)(zio)
      case false => zio.map(Chunk(_))
    }

  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = {
    def loop(variable: Ref[Int]) =
      whileLoop(variable.get.map(_ < 100)) {
        for {
          value <- variable.get
          _     <- putStrLn(s"At iteration: ${value}")
          _     <- variable.update(_ + 1)
        } yield ()
      }

    (for {
      variable <- Ref.make(0)
      _        <- loop(variable)
    } yield 0).exitCode
  }
}

object Iterate extends App {
  import zio.console._

  /**
   * EXERCISE
   *
   * Implement the `iterate` function such that it iterates until the condition
   * evaluates to false, returning the "last" value of type `A`.
   */
//  @tailrec
  def iterate[R, E, A](start: A)(cond: A => Boolean)(f: A => ZIO[R, E, A]): ZIO[R, E, A] =
    cond(start) match {
      case true => f(start).flatMap(iterate(_)(cond)(f))
      case false => f(start)
    }

  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    iterate(0)(_ < 100) { i =>
      putStrLn(s"At iteration: ${i}").as(i + 1)
    }.exitCode
}

object TailRecursive extends App {
  import zio.duration._

  trait Response
  trait Request {
    def returnResponse(response: Response): Task[Unit]
  }

  lazy val acceptRequest: Task[Request] = Task(new Request {
    def returnResponse(response: Response): Task[Unit] =
      Task(println(s"Returning response ${response}"))
  })

  def handleRequest(request: Request): Task[Response] = Task {
    println(s"Handling request ${request}")
    new Response {}
  }

  /**
   * EXERCISE
   *
   * Make this infinite loop (which represents a webserver) effectfully tail
   * recursive.
   */
  lazy val webserver: Task[Nothing] =
    acceptRequest flatMap { request =>
      handleRequest(request) flatMap { response =>
        request.returnResponse(response) flatMap { _ =>
          webserver
        }
      }
    }

  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    (for {
      fiber <- webserver.fork
      _     <- ZIO.sleep(100.millis)
      _     <- fiber.interrupt
    } yield ()).exitCode
}
