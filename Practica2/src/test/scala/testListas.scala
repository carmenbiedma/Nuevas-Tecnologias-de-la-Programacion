import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll,throws,AnyOperators}
import org.scalacheck.Gen._

object testListas extends Properties("testListas"){

  //Metodo de generacion de lista de valores
  val secuenciaEnteros = listOfN(10,choose(0,10))

  println(secuenciaEnteros.sample)

  property("longitud de lista") =
    forAll(secuenciaEnteros) {
      xs => {
        val lista : Lista[Int] = Lista ( xs: _*)
        val longitudList = xs.length
        val longitusLista = Lista.longitud(lista)
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

  property("producto enteros") =
    forAll(secuenciaEnteros){
      xs => {
        val lista : Lista[Int] = Lista ( xs: _*)
        val productoList = xs.map(x => x.toDouble).product
        val productoLista = Lista.productoEnteros(lista)

        productoList ?= productoLista
      }
    }

  val lista1 = listOf(choose(0,10))
  val lista2 = listOf(choose(0,10))

  property("concatenar listas") =
    forAll(lista1,lista2) {
      (xs1, xs2) => {
        val lista1: Lista[Int] = Lista(xs1: _*)
        val lista2: Lista[Int] = Lista(xs2:_*)
        val concatenarList = xs1 ++ xs2
        val concatenarLista = Lista.concatenar(lista1,lista2)

        val listaPrueba: Lista[Int] = Lista(concatenarList: _*)

        concatenarLista ?= listaPrueba
      }
    }


  property("sumaFoldRight") =
    forAll(secuenciaEnteros) {
      (xs) => {
        val lista : Lista[Int] = Lista ( xs: _*)
        val sumafoldRightList = xs.foldRight(0)((A,B) => A+B).toDouble
        val sumafoldRightLista = Lista.sumaFoldRight(lista)

        sumafoldRightList ?= sumafoldRightLista
      }
    }

  property("productoFoldRight") =
    forAll(secuenciaEnteros) {
      (xs) => {
        val lista : Lista[Int] = Lista ( xs: _*)
        val productofoldRightList = xs.foldRight(1)((A,B) => A*B).toDouble
        val productofoldRightLista = Lista.productoFoldRight(lista)

        productofoldRightList ?= productofoldRightLista
      }
    }

  property("asignar cabeza") =
    forAll(secuenciaEnteros) {
      (xs) => {
        val lista : Lista[Int] = Lista ( xs: _*)
        val asignarList = 1::xs
        val asignarLista = Lista.asignarCabeza(lista,1)

        val asignacionList:Lista[Int] = Lista (asignarList:_*)
        asignacionList ?= asignarLista
      }
    }

  property("eliminar cabeza") =
    forAll(secuenciaEnteros) {
      (xs) => {
        val lista : Lista[Int] = Lista ( xs: _*)
        val tailList = xs.tail
        val tailLista = Lista.tail(lista)

        val tailList2:Lista[Int] = Lista (tailList:_*)
        tailList2 ?= tailLista
      }
    }

  property("eliminar n primeros") =
    forAll(secuenciaEnteros) {
      (xs) => {
        val lista : Lista[Int] = Lista ( xs: _*)
        val eliminarNList = xs.drop(4)
        val eliminarNLista = Lista.eliminar(lista,4)

        val eliminarNList2:Lista[Int] = Lista (eliminarNList:_*)
        eliminarNList2 ?= eliminarNLista
      }
    }


  property("eliminar ultimo") =
    forAll(secuenciaEnteros) {
      (xs) => {
        val lista : Lista[Int] = Lista ( xs: _*)
        val eliminarUList = xs.dropRight(1)
        val eliminarULista = Lista.eliminarUltimo(lista)

        val eliminarUList2:Lista[Int] = Lista (eliminarUList:_*)
        eliminarUList2 ?= eliminarULista
      }
    }

  property("foldLeft") =
    forAll(secuenciaEnteros) {
      (xs) => {
        val lista : Lista[Int] = Lista ( xs: _*)
        val foldLeftList = xs.foldLeft(0)((B,A) => B+A)
        val foldLeftLista = Lista.foldLeft(lista,0)((B,A) => B+A)

        foldLeftList ?= foldLeftLista
      }
    }


}
