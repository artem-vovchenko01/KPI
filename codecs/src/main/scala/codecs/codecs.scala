package codecs

import com.sun.org.apache.xerces.internal.impl.xpath.regex.Match

import scala.annotation.tailrec

/**
  * A data type modeling JSON values.
  *
  * For example, the `42` integer JSON value can be modeled as `Json.Num(42)`
  */
sealed trait Json {
  /**
   * Try to decode this JSON value into a value of type `A` by using
   * the implicit decoder.
   *
   * Note that you have to explicitly fix `A` type parameter when you call the method:
   *
   * {{{
   *   someJsonValue.decodeAs[User] // OK
   *   someJsonValue.decodeAs       // Wrong!
   * }}}
   */
  def decodeAs[A](implicit decoder: Decoder[A]): Option[A] = decoder.decode(this)
}

object Json {
  /** The JSON `null` value */
  case object Null extends Json
  /** JSON boolean values */
  case class Bool(value: Boolean) extends Json
  /** JSON numeric values */
  case class Num(value: BigDecimal) extends Json
  /** JSON string values */
  case class Str(value: String) extends Json
  /** JSON objects */
  case class Obj(fields: Map[String, Json]) extends Json
  /** JSON arrays */
  case class Arr(items: List[Json]) extends Json
}

/**
  * A type class that turns a value of type `A` into its JSON representation.
  */
trait Encoder[-A] {

  def encode(value: A): Json

    /**
    * Transforms this `Encoder[A]` into an `Encoder[B]`, given a transformation function
    * from `B` to `A`.
    *
    * For instance, given a `Encoder[String]`, we can get an `Encoder[UUID]`:
    *
    * {{{
    *   def uuidEncoder(given stringEncoder: Encoder[String]): Encoder[UUID] =
    *     stringEncoder.transform[UUID](uuid => uuid.toString)
    * }}}
    *
    * This operation is also known as “contramap”.
    */
  def transform[B](f: B => A): Encoder[B] =
    Encoder.fromFunction[B](value => this.encode(f(value)))
}

object Encoder extends EncoderInstances {

  /**
   * Convenient method for creating an instance of encoder from a function `f`
   */
  def fromFunction[A](f: A => Json) = new Encoder[A] {
    def encode(value: A): Json = f(value)
  }

}

trait EncoderInstances {

  /** An encoder for the `Unit` value */
  implicit val unitEncoder: Encoder[Unit] =
    Encoder.fromFunction(_ => Json.Null)

  /** An encoder for `Int` values */
  implicit val intEncoder: Encoder[Int] =
    Encoder.fromFunction(n => Json.Num(BigDecimal(n)))

  /** An encoder for `String` values */
  implicit val stringEncoder: Encoder[String] =
    Encoder.fromFunction(s => Json.Str(s))

  /** An encoder for `Boolean` values */
  implicit val boolEncoder: Encoder[Boolean] = 
    Encoder.fromFunction(b => Json.Bool(b))

  /**
    * Encodes a list of values of type `A` into a JSON array containing
    * the list elements encoded with the given `encoder`
    */
  implicit def listEncoder[A](implicit encoder: Encoder[A]): Encoder[List[A]] =
    Encoder.fromFunction(as => Json.Arr(as.map(encoder.encode)))

}

/**
  * A specialization of `Encoder` that returns JSON objects only
  */
trait ObjectEncoder[-A] extends Encoder[A] {
  // Refines the encoding result to `Json.Obj`
  def encode(value: A): Json.Obj

  /**
    * Combines `this` encoder with `that` encoder.
    * Returns an encoder producing a JSON object containing both
    * fields of `this` encoder and fields of `that` encoder.
    */
  def zip[B](that: ObjectEncoder[B]): ObjectEncoder[(A, B)] =
    ObjectEncoder.fromFunction { case (a, b) =>
      Json.Obj(this.encode(a).fields ++ that.encode(b).fields)
    }
}



object ObjectEncoder {

  /**
    * Convenient method for creating an instance of object encoder from a function `f`
    */
  def fromFunction[A](f: A => Json.Obj): ObjectEncoder[A] = new ObjectEncoder[A] {
    def encode(value: A): Json.Obj = f(value)
  }

  /**
    * An encoder for values of type `A` that produces a JSON object with one field
    * named according to the supplied `name` and containing the encoded value.
    */
  def field[A](name: String)(implicit encoder: Encoder[A]): ObjectEncoder[A] =
    ObjectEncoder.fromFunction(a => Json.Obj(Map(name -> encoder.encode(a))))

}

/**
  * The dual of an encoder. Decodes a serialized value into its initial type `A`.
  */
trait Decoder[+A] {
  /**
    * @param data The data to de-serialize
    * @return The decoded value wrapped in `Some`, or `None` if decoding failed
    */
  def decode(data: Json): Option[A]

  /**
    * Combines `this` decoder with `that` decoder.
    * Returns a decoder that invokes both `this` decoder and `that`
    * decoder and returns a pair of decoded value in case both succeed,
    * or `None` if at least one failed.
    */
  def zip[B](that: Decoder[B]): Decoder[(A, B)] =
    Decoder.fromFunction { json =>
      this.decode(json).zip(that.decode(json))
    }

  /**
    * Transforms this `Decoder[A]` into a `Decoder[B]`, given a transformation function
    * from `A` to `B`.
    *
    * This operation is also known as “map”.
    */
  def transform[B](f: A => B): Decoder[B] =
    Decoder.fromFunction(json => this.decode(json).map(f))
}

object Decoder extends DecoderInstances {

  /**
    * Convenient method to build a decoder instance from a function `f`
    */
  def fromFunction[A](f: Json => Option[A]): Decoder[A] = new Decoder[A] {
    def decode(data: Json): Option[A] = f(data)
  }

  /**
    * Alternative method for creating decoder instances
    */
  def fromPartialFunction[A](pf: PartialFunction[Json, A]): Decoder[A] =
    fromFunction(pf.lift)

}

trait DecoderInstances {

  /** A decoder for the `Unit` value */
  implicit val unitDecoder: Decoder[Unit] =
    Decoder.fromPartialFunction { case Json.Null => () }

  /** A decoder for `Int` values. Hint: use the `isValidInt` method of `BigDecimal`. */
  implicit val intDecoder: Decoder[Int] = 
    Decoder.fromPartialFunction { case Json.Num(x) if x.isValidInt => x.toInt }
    // if BigDecimal.isValidInt(x.value) 

  /** A decoder for `String` values */
  implicit val stringDecoder: Decoder[String] = 
    Decoder.fromPartialFunction { case x: Json.Str => x.value }

  /** A decoder for `Boolean` values */
  implicit val boolDecoder: Decoder[Boolean] = 
    Decoder.fromPartialFunction { case x: Json.Bool => x.value }

  /**
    * A decoder for JSON arrays. It decodes each item of the array
    * using the given `decoder`. The resulting decoder succeeds only
    * if all the JSON array items are successfully decoded.
    */
  implicit def listDecoder[A](implicit decoder: Decoder[A]): Decoder[List[A]] = 
    Decoder.fromPartialFunction { case arr: Json.Arr => arr.items.map(i => decoder.decode(i).get) }

  /**
    * A decoder for JSON objects. It decodes the value of a field of
    * the supplied `name` using the given `decoder`.
    */
  def field[A](name: String)(implicit decoder: Decoder[A]): Decoder[A] = 
    Decoder.fromFunction //{ case obj: Json.Obj => decoder.decode(obj.fields(name)).get }
    {
      case Json.Obj(mp) => mp.get(name) match {
        case None => None 
        case v => v.get.decodeAs[A]
      }
    }

}

case class Person(name: String, age: Int)

object Person extends PersonCodecs

trait PersonCodecs {

  /** The encoder for `Person` */
  implicit val personEncoder: Encoder[Person] =
    ObjectEncoder.field[String]("name")
      .zip(ObjectEncoder.field[Int]("age"))
      .transform[Person](user => (user.name, user.age))

  /** The corresponding decoder for `Person` */
  // TODO Define he decoder for `Person`
  implicit val personDecoder: Decoder[Person] = 
    // Decoder.fromPartialFunction { case j: Json.Obj => 
    //   Person(j.fields("name").decodeAs[String].get, j.fields("age").decodeAs[Int].get) }

    Decoder.field[String]("name")
      .zip(Decoder.field[Int]("age"))
      .transform[Person](j => Person(j._1, j._2))
}

case class Contacts(people: List[Person])

object Contacts extends ContactsCodecs

trait ContactsCodecs {

  // TODO Define the encoder and the decoder for `Contacts`
  // The JSON representation of a value of type `Contacts` should be
  // a JSON object with a single field named “people” containing an
  // array of values of type `Person` (reuse the `Person` codecs)

  /** The encoder for `Contacts` */
  implicit val contactEncoder: Encoder[Contacts] =
    ObjectEncoder.field[List[Person]]("people")
    .transform[Contacts](contacts => contacts.people)

  /** The decoder for `Contacts` */
  implicit val contactDecoder: Decoder[Contacts] =
    Decoder.fromPartialFunction { case j: Json.Obj => 
        Contacts(j.fields("people").decodeAs[List[Person]].get) }
}

// individual task
case class Author(name: String, surname: String)

object Author extends AuthorCodecs

trait AuthorCodecs {
  /** Encoder for `Author` */
  implicit val authorEncoder: Encoder[Author] =
    ObjectEncoder.field[String]("name")
      .zip(ObjectEncoder.field[String]("surnme"))
      .transform[Author](author => (author.name, author.surname))

  /** The corresponding decoder for `Author` */
  implicit val authorDecoder: Decoder[Author] = 
    Decoder.field[String]("name")
      .zip(Decoder.field[String]("surname"))
      .transform[Author](j => Author(j._1, j._2))
}

// In case you want to try your code, here is a simple `Main`
// that can be used as a starting point. Otherwise, you can use
// the REPL (use the `console` sbt task).
object Main {
  import Util._

  def main(args: Array[String]): Unit = {
    /** renderJson */
    println("Testing renderJson")
    println(renderJson(42))
    println(renderJson("foo"))

    val maybeJsonString = parseJson(""" "foo" """)
    val maybeJsonObj    = parseJson(""" { "name": "Alice", "age": 42 } """)
    val maybeJsonObj2   = parseJson(""" { "name": "Alice", "age": "42" } """)

    /** Testing different decoders */
    println("\nTesting decoders")
    println(maybeJsonString.flatMap(_.decodeAs[Int]))
    println(maybeJsonString.flatMap(_.decodeAs[String]))
    println(maybeJsonObj.flatMap(_.decodeAs[Person]))
    println(maybeJsonObj2.flatMap(_.decodeAs[Person]))
    println(renderJson(Person("Bob", 66)))

    /** Testing Contacts */
    println("\nTesting contacts")
    val contacts = Contacts(List(Person("Artem", 20), Person("John", 25)))
    val contactsJson = renderJson(contacts)
    println(contactsJson)
    val parsedContacts = parseJson(contactsJson)
    println(parsedContacts)
    println(parsedContacts.flatMap(_.decodeAs[Contacts]))

    // val decoder = implicitly[Decoder[Contacts]]
    // val decoder = Decoder.decode(Contacts.contactDecoder.decode)
  }

}
