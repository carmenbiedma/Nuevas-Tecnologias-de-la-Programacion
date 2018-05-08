
/**
  * Objeto singleton para probar la funcionalidad del triangulo
  * de Pascal
  */
object Funciones {
  /**
    * Metodo main: en realidad no es necesario porque el desarrollo
    * deberia guiarse por los tests de prueba
    *
    * @param args
    */
  def main(args: Array[String]) {
    println("................... Triangulo de Pascal ...................")

    // Se muestran 10 filas del trinagulo de Pascal
    for (row <- 0 to 10) {
      // Se muestran 10 10 columnas
      for (col <- 0 to row)
        print(calcularValorTrianguloPascal(row, col) + " ")

      // Salto de linea final para mejorar la presentacion
      println()
    }

    // Se muestra el valor que debe ocupar la columna 5 en la fila 10
    println(calcularValorTrianguloPascal(15, 10))
    println(calcularValorTrianguloPascal(0, 0))

    val cadenaPrueba = List[Char]('(', 'a', '(', ')', ')')
    println(chequearBalance(cadenaPrueba))

    val monedasPrueba = List[Int](1, 2, 5, 10)
    println(contarCambiosPosibles(23, monedasPrueba))

    val arrayPrueba = Array[Int](0,8)
    println(busquedaBinaria[Int](arrayPrueba,8, (x, y) => x < y))

  }

  /**
    * Ejercicio 1: funcion para generar el triangulo de Pascal
    *
    * @param columna
    * @param fila
    * @return
    */
  def calcularValorTrianguloPascal(fila: Int, columna: Int): Int = {

    if (columna == 0 || columna == fila) 1
    else calcularValorTrianguloPascal(fila - 1, columna - 1) + calcularValorTrianguloPascal(fila - 1, columna)

  }

  /**
    * Ejercicio 2: funcion para chequear el balance de parentesis
    *
    * @param cadena cadena a analizar
    * @return valor booleano con el resultado de la operacion
    */
  def chequearBalance(cadena: List[Char]): Boolean = {
    def go(cadena: List[Char], cont: Int): Boolean = {
      if (cadena.isEmpty && cont == 0) true
      else if (cadena.isEmpty && cont != 0) false
      else if (cadena.head == '(') go(cadena.tail, cont + 1)
      else if (cadena.head == ')') go(cadena.tail, cont - 1)
      else go(cadena.tail, cont)
    }

    go(cadena, 0)
  }

  /**
    * Ejercicio 3: funcion para determinar las posibles formas de devolver el
    * cambio de una determinada cantidad con un conjunto de monedas
    *
    * @param cantidad
    * @param monedas
    * @return contador de numero de vueltas posibles
    */
  def contarCambiosPosibles(cantidad: Int, monedas: List[Int]): Int = {

    def go(cantidad: Int, monedas: List[Int], contador: Int): Int = {

      if (cantidad == 0) contador + 1
      else if (monedas.isEmpty) contador
      else {
        val moneda = monedas.head
        if (cantidad >= moneda) {
          var acum = contador
          for (i <- 0 to cantidad / moneda)
            acum = go(cantidad - i * moneda, monedas.tail, acum)
          acum
        } else go(cantidad, monedas.tail, contador)
      }


      /* FORMA 1
       caso base 1 -> cantidad == 0 => contador+1
       caso base 2 -> lista de monedas vacía => contador
       caso inductivo ->
                          moneda = monedas.head //primera moneda disponible
                          IF(cantidad>=moneda){

                            var acum = contador
                            for(i<-0 to cantidad/moneda)
                              acum=go(canditad - i*moneda,monedas.tail,acum)
                            acum

                          } ELSE go(cantidad,monedas.tail,contador)


      */

      /*
       FORMA 2: MAS FUNCIONAL

          if(cantidad == 0) 1
          else {
              if(monedas.isEmpty) 0
              else if(cantidad >= monedas.head){
                (0 to cantidad/monedas.head).map(num = > contadorCambiosPosibles(cantidad-num*moneda.head,monedas.tail)).foldLeft(0)(_+_)
              } else {
                contadorCambiosPosibles(cantidad,monedas.tail)
              }


          }
       */

    }

    go(cantidad, monedas, 0)
  }



  /**
    * Metodo generico para busqueda binaria
    *
    * @param coleccion conjunto de datos sobre los que buscar
    * @param aBuscar   elemento a buscar
    * @param criterio  para comparar dos elementos de tipo A
    * @tparam A parametro de tipo
    * @return posicion del valor buscado o -1 en caso de no hallarlo
    */
  def busquedaBinaria[A](coleccion: Array[A], aBuscar: A, criterio: (A, A) => Boolean): Int = {
    def go(izq: Int, dcha: Int): Int = {
      if (coleccion((izq+dcha) / 2) == aBuscar) (izq+dcha)/2
      else if(dcha>(izq+1)){
        if(criterio(coleccion((izq+dcha)/2),aBuscar)) go((izq+dcha)/2,dcha)
        else go(izq,(izq+dcha)/2)
      } else -1
    }

    go(0, coleccion.length)

  }
}