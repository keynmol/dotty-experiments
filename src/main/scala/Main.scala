import scala.deriving._
import compiletime._

object Main:
 def main(args: Array[String]): Unit =
    import Encoders.{given _}
    import JsonSyntax.{given _, _}

    enum Bla derives Encoder {
      case CaseOne(b: String)
      case CaseTwo(v: Int, vec: Vector[String])
    }

    println(summon[Encoder[Bla]].asJson(Bla.CaseOne("hello")))
    
    println(summon[Encoder[Bla]].asJson(Bla.CaseTwo(5, Vector("hello"))))

sealed trait Json
case object JsNull extends Json
case class JsString(raw: String) extends Json
case class JsArray(values: Vector[Json]) extends Json
case class JsObject(values: Map[String, Json]) extends Json
case class JsNumber(value: BigDecimal) extends Json

trait Decoder[A]: 
  def fromJson(value: Json): Either[Throwable, A] 
 
object Json:
  def serialise(value: Json): String =
    value match
      case JsNull          => "null"
      case JsString(raw)   => '"'.toString + raw + '"'.toString
      case JsArray(values) => "[" + values.map(serialise).mkString(",") + "]"
      case JsObject(values) => 
        "{" + 
            values.map { (key, value) => 
              serialise(JsString(key)) + ":" + serialise(value)
            }.mkString(",") + 
        "}"
      case JsNumber(value) => value.toString

 
trait Encoder[A]:
  def asJson(value: A): Json

object Encoder:
  inline def summonAll[T <: Tuple]: List[Encoder[_]] = inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts)  => summonInline[Encoder[t]] :: summonAll[ts]

  inline def summonLabels[T <: Tuple]: List[String] = inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts) => constValue[t].asInstanceOf[String] :: summonLabels[ts]

  case class Test(vec: Vector[String])

  val a = Test(Vector.empty)
 
  def iterator[T](p: T) = p.asInstanceOf[Product].productIterator

  def encProduct[T](p: Mirror.ProductOf[T], encoders: List[Encoder[_]], labels: List[String]): Encoder[T] =
    new Encoder[T]:
      def asJson(value: T): Json =
       JsObject(
         iterator(value).zip(labels).zip(encoders).map { case((v, label), encoder) => 
            label -> encoder.asInstanceOf[Encoder[Any]].asJson(v)
          }.toMap
      )

  def encSum[T](p: Mirror.SumOf[T], elems: List[Encoder[_]]): Encoder[T] =
    new Encoder[T]:
      def asJson(value: T): Json =
        println(elems(p.ordinal(value)).asInstanceOf[Encoder[Any]].asJson(value))
        JsString(p.ordinal(value).toString)

  
  inline given derived[T](using m: Mirror.Of[T]) as Encoder[T] =
    val elemInstances = summonAll[m.MirroredElemTypes]
    val elemLabels = summonLabels[m.MirroredElemLabels]

    inline m match
      case m: Mirror.ProductOf[T] =>
        encProduct(m, elemInstances, elemLabels)
      case s: Mirror.SumOf[T] =>
        encSum(s, elemInstances)


object Decoders:
  given stringDecoder as Decoder[String]:
    def fromJson(j: Json) = j match
      case JsString(value)   => Right(value)
      case other             => 
       Left(RuntimeException(s"Unexpected $other (expected String)"))

  given vecDec[T](using valueDec: Decoder[T]) as Decoder[Vector[T]]: 
    def fromJson(j: Json) = j match
      case JsArray(values) =>
        values
          .map(valueDec.fromJson)
          .foldRight[Either[Throwable, Vector[T]]](Right(Vector.empty[T])) {
            (next, acc) =>               acc.flatMap(current => next.map(res => res +: current))
          }
 
      case other => 
        Left(RuntimeException(s"Unexpected $other (expected JsArray)"))

object Encoders:
  given stringJson as Encoder[String]:
    def asJson(value: String) = JsString(value)

  given intJson as Encoder[Int]:
    def asJson(value: Int) = JsNumber(BigDecimal.apply(value))

  given vecEnc[T](using valueEnc: Encoder[T]) as Encoder[Vector[T]]:
    def asJson(value: Vector[T]) = JsArray(value.map(valueEnc.asJson))

object JsonSyntax:
  def [A](obj: A).asJson(using enc: Encoder[A]): Json = enc.asJson(obj)
  def (j: Json).serialise: String = Json.serialise(j)
  def [A](j: Json).as(using dec: Decoder[A]): Either[Throwable, A] = dec.fromJson(j) 
 
