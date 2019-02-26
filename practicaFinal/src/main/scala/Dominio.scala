class Dominio(val variables:List[Variable]) {

    val indices  = calcularIndices()
    val pesos= calcularPesos()


  /**
    * Método para calcular el peso de una variable
    * @param indice : índice de la varibale a la que se le calcula el peso
    * @return peso de la variable.
    */

    def calcularPeso(indice:Int):Int = {
      (indice+1 until variables.length).map(i => variables(i).n_estados).reduceLeft(_*_)
    }

  /**
    * Método para calcular los pesos de todas las variables
    * @return Devuelve un map que relaciona cada variable con su peso
    */

  def calcularPesos(): Map[Variable,Int] = {
      (0 until variables.length).map(i => {
        if(i==variables.length-1) variables(i) -> 1
        else variables(i) -> calcularPeso(i)
      }).toMap
    }

  /**
    * Calcula el índice asociado a cada variable
    * @return Diccionario que tiene como clave un entero y por valor su correspondiente variable
    */

    def calcularIndices() : Map[Int,Variable] = {
      var ind = Map[Int,Variable]()
      var aux = 0;
      variables.foreach(i => {
        ind+=(aux->i)
        aux= aux+1
      })
      ind
    }

  /**
    * Nos informa de si una variable existe en el dominio
    * @param variable
    * @return true si existe, false en caso contrario
    */

  def existe(variable: Variable):Boolean={
    var retorno=false
    variables.foreach(x=> {
      if(x.nombre==variable.nombre) retorno=true
    })
    retorno
  }

  /**
    * Sobrecarga del metodo toString
    * @return
    */

  override def toString: String = {
    "En clase Dominio" + variables.map(i => i.nombre+ "( s : " + i.n_estados + " w : " + pesos(i) + " ) ").mkString("") +"\n"
  }

  /**
    * Método para saber si un dominio está vacío
    * @return
    */

  def vacio : Boolean = variables.isEmpty

  /**
    * Método para saber el número de variables que forman el dominio
    * @return
    */

  def longitud : Int = variables.length

  /**
    * Método que calcula el máximo índice posible del dominio
    * @return
    */

  def maximoIndice : Int = {
    val maximoPeso = variables(0)
    pesos(maximoPeso) * maximoPeso.n_estados
  }

  /**
    * Método para acceder a una variable de índice (i) de la forma
    * @param index : Indice de la variable que queremos devolver.
    * @return
    */

  def apply (index:Int) : Variable = variables(index)

  /**
    * Sobrecarga del operador + que añade una variable al final del dominio
    * @param variable : Variable que queremos añadir al dominio
    * @return nuevo dominio resultante de añadir la variable pasada como argumento al dominio this
    */

  def +(variable:Variable):Dominio={
    if(!existe(variable)){
      Dominio(variables:::List(variable))
    }
    else this
  }

  /**
    * Sobrecarga del operador + para sumar dos dominios
    * @param dominio : Dominio que queremos sumar al dominio this
    * @return un nuevo dominio con la suma de this y el pasado como parametro
    */

  def +(dominio:Dominio): Dominio={
    Dominio((variables union dominio.variables).distinct.toList)
  }


  /**
    * Sobrecarga del operador - para eliminar una variable del dominio
    * @param variable : Variable que queremos eliminar
    * @return
    */

    def - (variable:Variable) : Dominio = {
      var d = List[Variable]()

      variables.foreach(i => if(i.nombre!=variable.nombre) d++=List(i))

      val res = Dominio(d)
      res
    }

}

object Dominio {

  def apply(v:List[Variable]): Dominio = {
    new Dominio(v)
  }

}

object pruebaDominio extends App {

  // Se crea dominio vacio
   val dominioVacio=Dominio(List())

   // Se comprueba que funciona el metodo asociado a comprobar la condicion
   // de dominio vacio
   println("Comprobacion de vacio sobre dominio vacio: "+ dominioVacio.vacio)

   // Se crean 4 variables
  val X1 = Variable("X1",3)
   val X2 = Variable("X2",4)
   val X3 = Variable("X3",2)
   val X4 = Variable("X4",2)

   // Se crea un dominio con las variables creadas antes
   val dominioNoVacio=Dominio(List(X1,X2,X3,X4))

   // Este dominio ya no esta vacio
   println("Comprobacion de vacio sobre dominio no vacio: "+dominioNoVacio.vacio)

   // Se obtiene la longitud del dominio
   val longitud=dominioNoVacio.longitud
   println("Longitud del dominio no vacio (debe ser 4): "+longitud)

   // Se muestra el objeto usando toString
   println(dominioNoVacio)

   // Se suma una variable al dominioNoVacio
   val X5 = Variable("X5", 5)
   val dominioSuma=dominioNoVacio+X5
   println("Dominio suma (+X5): "+dominioSuma)

   // Se crea un dominio sobre X4, X5 y X6
   val X6 = Variable("X6", 3)
   val dominioNoVacio2=Dominio(List(X1, X2, X5, X6))

   // Se genera ahora la suma de los dos dominios no vacios
   val dominioSuma2=dominioNoVacio + dominioNoVacio2
   println("Suma de dominios: "+dominioSuma2)

   // Prueba de calculo del maximo indice

   val maximoIndice=dominioSuma2.maximoIndice
   println("Maximo indice de dominio de suma: "+ maximoIndice)

  val diferencia = dominioNoVacio-X4
  println("DIFERENCIA = " + diferencia)

}


