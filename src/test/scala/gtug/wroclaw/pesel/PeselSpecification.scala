package gtug.wroclaw.pesel

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

import com.fortysevendeg.scalacheck.datetime.jdk8.GenJdk8
import com.github.przemek_pokrywka.pesel.Pesel
import org.scalacheck.Prop.forAll
import org.scalacheck.Test.Parameters
import org.scalacheck.{Arbitrary, Gen, Properties}

import scala.collection.immutable
import scala.util.Random

object PeselSpecification extends Properties("Pesel") {

  def returnValidChecksum(peselWithoutLast: String): Option[Int] = {
    var p: Pesel = null // local nulls are fine

    var maybeCheckSum: Option[Int] = None

    do {
      val randomIntUpTo9 = new Random().nextInt(10)
      maybeCheckSum = Some(randomIntUpTo9)

      p = Pesel(s"$peselWithoutLast$randomIntUpTo9")
    } while (!p.valid) // TODO does it makes sense to use the same method????
    maybeCheckSum
  }

  implicit val peselGen: Gen[Pesel] = for {
    date: ZonedDateTime <- GenJdk8.genZonedDateTime
    fourChars: Seq[Char] <- Gen.listOfN(4, Gen.numChar)
    fourCharsCluster: String = fourChars.map(_.toString).reduce(_ + _)

    formattedDate = date.format(DateTimeFormatter.ofPattern("yyMMdd"))
    c5 = returnValidChecksum(formattedDate + fourCharsCluster).get

    ending = s"$fourCharsCluster$c5"
  } yield Pesel(formattedDate + ending)

  implicit val arbPesel: Arbitrary[Pesel] = Arbitrary(peselGen)
  implicit val params: Parameters = Parameters.default.withMinSize(1000).withMaxSize(10000)

  property("pesel is valid") = forAll { pesel: Pesel =>
    pesel.valid
  }

}
