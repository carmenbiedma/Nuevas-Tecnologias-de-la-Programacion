import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.{AnyOperators, forAll, throws}
import org.scalacheck.Gen._

object testFunciones extends Properties("testFunciones"){

  val coordenadasExtremos = for {
    fila <- Gen.choose(0, 10)
    columna <- Gen.oneOf(0, fila)
  }yield (fila, columna)


  property("Elementos en lados del triangulo valen 1") = {
    forAll(coordenadasExtremos) { (i) => {
      val resultado = Funciones.calcularValorTrianguloPascal(i._1, i._2)
      resultado ?= 1
      }
    }
  }

  val coordenadasCentro = for {
    fila <- Gen.choose(2, 9)
    columna <- Gen.oneOf(1, fila-1)
  }yield (fila, columna)

  property("Elementos en centro del triangulo") = {
    forAll(coordenadasCentro) { (i) => {
      val resultado = Funciones.calcularValorTrianguloPascal(i._1, i._2)
      val comprobacion = Funciones.calcularValorTrianguloPascal(i._1-1,i._2-1)  + Funciones.calcularValorTrianguloPascal(i._1-1,i._2)
      resultado ?= comprobacion
    }
    }
  }


  property("Cambio de monedas") = {
    val monedas = List[Int](1, 2, 5, 10)
    val cambiode23 = 52

    val resultado = Funciones.contarCambiosPosibles(23,monedas)

    resultado == cambiode23
  }

  val strGen =
    (n: Int) =>
      Gen.listOfN(n, Gen.oneOf('(',')',Gen.alphaChar.sample.get)).
        map(_.mkString)

  val cadenaString = strGen(20)


  property("chequear parentesis") = {
    forAll(cadenaString) { (i) => {
      val abriendo = i.count(_ == '(')
      val cerrando = i.count(_ == ')')
      val lista = i.toList
      val prueba = Funciones.chequearBalance(lista)

      val diff = abriendo - cerrando

      if(diff == 0)
        prueba ?= true
      else
        prueba ?= false

    }
    }
  }

  val secuenciaEnteros = listOfN(10,choose(0,10))

  property("busqueda binaria") = {
    forAll(secuenciaEnteros) { (i) => {
      val value:Int = 8
      val array: Array[Int] = i.toArray
      scala.util.Sorting.quickSort(array)
      val array2 = array.distinct
      val prueba = array2.indexOf(value)
      val posFuncion = Funciones.busquedaBinaria[Int](array2, value, (x: Int, y: Int) => x < y)

      prueba ?= posFuncion
    }
    }
  }
}
