import cats.effect.{IO, IOApp}

object CatsExample extends IOApp.Simple {
  val run = IO.println("Hello, World in Cats!")
}
