import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll,throws,AnyOperators}
import org.scalacheck.Gen._

object ListaTest extends Properties("ListaTest"){

  //Metodo de generacion de lista de valores
  val secuenciaEnteros = listOf(choose(0,10))

  println(secuenciaEnteros.sample)

  property("longitud de lista") =
    forAll(secuenciaEnteros) {
      xs => {
        //Para cada secuencia cualquiera dada
        //Voy a crear una lista a parrtir de xs
        val lista : Lista[Int] = Lista ( xs: _*)
        val longitudList = xs.length
        val longitusLista = Lista.longitud(lista)
        //?= nos da el resultado de la comprobacion aunque no hagamos un println
        longitudList ?= longitusLista
      }
    }

  property("suma enteros") =
    forAll(secuenciaEnteros){
      xs => {
        val lista : Lista[Int] = Lista ( xs: _*)
        val sumaList = xs.map(x => x.toDouble).sum
        val sumaLista = Lista.sumaEnteros(lista)

        sumaList ?= sumaLista
      }
    }
}
