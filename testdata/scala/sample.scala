package com.example.app

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util._
import akka.actor.{Actor, ActorSystem, Props}
import java.time.Instant
import given scala.math.Ordering.Int
import io.circe.{Encoder, Decoder}

/** Type alias for common map type. */
type JsonMap = Map[String, Any]

/** Enum for message priority (Scala 3). */
enum Priority:
  case Low, Medium, High, Critical

/** Sealed trait for application events. */
sealed trait AppEvent
case class UserCreated(id: String, name: String) extends AppEvent
case class UserDeleted(id: String) extends AppEvent
case class MessageSent(from: String, to: String, content: String) extends AppEvent

/** Trait for serialization. */
trait Serializable[T]:
  def serialize(value: T): String
  def deserialize(data: String): T

/** Trait for logging. */
trait Logging:
  def log(message: String): Unit =
    println(s"[${Instant.now}] $message")

/** Abstract class for base repository. */
abstract class BaseRepository[T](tableName: String):
  def findById(id: String): Future[Option[T]]
  def findAll(): Future[Seq[T]]
  def save(entity: T): Future[Unit]

/** Case class for user model. */
case class User(
  id: String,
  name: String,
  email: String,
  role: String = "viewer"
)

/** Companion object for User. */
object User:
  def fromMap(data: Map[String, String]): User =
    User(
      id = data("id"),
      name = data("name"),
      email = data("email"),
      role = data.getOrElse("role", "viewer")
    )

/** Sealed class for results. */
sealed class Result[+T]
class Success[T](val value: T) extends Result[T]
class Failure(val error: String) extends Result[Nothing]

/** Main service class. */
class UserService(repo: BaseRepository[User]) extends Logging:
  def getUser(id: String): Future[Option[User]] =
    log(s"Fetching user $id")
    repo.findById(id)

  def createUser(name: String, email: String): Future[Unit] =
    val user = User(java.util.UUID.randomUUID().toString, name, email)
    repo.save(user)

  private def validateEmail(email: String): Boolean =
    email.contains("@")

/** Object for configuration. */
object AppConfig:
  val defaultTimeout: Int = 30
  val maxRetries: Int = 3

  def load(path: String): Map[String, String] =
    Map.empty

/** Actor for handling messages. */
class MessageActor extends Actor:
  def receive: Receive =
    case msg: String => println(s"Received: $msg")
    case _ => println("Unknown message")

/** Top-level val and var declarations. */
val appVersion: String = "1.0.0"
var requestCount: Int = 0

/** Top-level def. */
def initialize(): Unit =
  println("Initializing application")

/** Private top-level def. */
private def setupInternal(): Unit =
  println("Internal setup")

/** Given instance (Scala 3). */
given userOrdering: Ordering[User] = Ordering.by(_.name)
