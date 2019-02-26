class Asignacion(val dominio:Dominio, val valores: List[Int]) {
  val datos = calcularDatos()

  require(valoresValidos)

  /**
    * Método para comprobar que los valores pasados como parámetro son correctos, es decir,
    * que ningun valor de los pasados para asignarselo a una variable sea mayor que su número de estados.
    * Será un requerimiento para poder crear un elemento de la clase
    * @return true si se cumple para todos los valores y false en caso contrario
    */


  private def valoresValidos() : Boolean = {
    var flag = true

    if(!dominio.vacio) {
      val resultado = (0 until dominio.longitud).find(indice =>
        valores(indice) >= dominio(indice).n_estados).getOrElse(null)

      flag = (resultado == null)
    }
    if(!flag){
      println("Asignacion no valida")
      println(dominio)
      print(valores)
    }
    flag
  }

  /**
    * Asocia su valor de estado a cada variable en un map que tiene como clave la variable y su estado como valor
    * @return
    */

  def calcularDatos() : Map[Variable,Int] = (0 until dominio.longitud).map(i =>dominio(i) -> valores(i)).toMap


  /**
    * Sobrecarga del operador toString
    * @return
    */

  override def toString: String = {
    var s:String = ""
    dominio.variables.foreach(i => s+= "[ " +  i.nombre + " - " + datos(i) + " ]  ")
    "En clase Asignacion" + s
  }

  /**
    * Devuelve si una asignación está vacía
    * @return
    */

  def vacia : Boolean = return dominio.vacio

  /**
    * Devuelve el número de variables que forman la asignación
    * @return
    */

  def obtenerNumeroVariables : Int = dominio.longitud

  /**
    * Muestra el valor/estado que tiene asociada la variable que pasamos por parámetro
    * @param v : variable a consultar el estado
    * @return estado de la variable
    */

  def obtenerValorVariable (v : Variable) : Int = datos(v)

  /**
    * Sobrecarga del operador + para poder añadir un par (variable,valor) a la asignación
    * @param par Tupla con la variable y el valor que queremos añadir
    * @return Devuelve una nueva asignación con el reslutado
    */

  def +(par:Tuple2[Variable,Int]):Asignacion={
    var conjun=this.dominio.variables
    conjun++=List[Variable](par._1)
    var valor=this.valores
    valor++=List[Int](par._2)
    Asignacion(Dominio(conjun),valor)

  }

  /**
    * Dada una asignación completa, calcula el índice del valor asociado a dicha asignación
    * @return Índice que almacena el valor de la asignacion
    */

  def calcularIndice():Int={
    (0 until valores.length).map(i=> dominio.pesos(dominio.indices(i))*valores(i)).reduceLeft(_+_)
  }

  /**
    * Devuelve la asignación correspondiente a un dominio
    * @param d : Dominio del que se quiere saber la asignación
    * @return Una nueva asignación que contiene solo las variables del dominio pasado como argumento y sus estados
    */

  def proyectar(d:Dominio) : Asignacion = {
    val lista = d.variables.map(i => {
      this.obtenerValorVariable(i)
    }).toList
    Asignacion(d,lista)
  }




}

object Asignacion {

  /**
    * Crea una asignación a partir de un dominio y una lista de valores
    * @param d : Dominio
    * @param v : Valores doubles asociados a cada una de las ocmbinaciones de variables
    * @return Objeto asignación creado
    */
  def apply(d: Dominio, v: List[Int]): Asignacion = new Asignacion(d, v)

  /**
    * Crea una asignación con valores a 0
    * @param d : Dominio de la asignación
    * @return Objeto asignación creado
    */

  def apply(d:Dominio) : Asignacion = {
    new Asignacion(d,List.fill(d.longitud)(0))
  }


  /**
    * Crea una asignación a partir de un índice pasado como argumento.
    * @param d : Dominio de la asignación
    * @param i : Índice resultante de la asignación
    * @return Objeto asignación creado
    */
  def apply(d:Dominio, i:Int) : Asignacion = {
    val valores = (0 until d.longitud).map(indiceVariable =>{
      val variable=d(indiceVariable)
      (i/d.pesos(variable)) % variable.n_estados
    }).toList

    Asignacion(d,valores)
  }
}

object pruebaAsignacion extends App {

  // Se crea asignacion vacia y se comprueba su chequeo
   val asignacionVacia=Asignacion(Dominio(List()), List())
   println("Comprobacion vacio asignacion vacia: "+asignacionVacia.vacia)

   // Se crean 4 variables con diferentes estados
  val X1 = Variable("X1",3)
  val X2 = Variable("X2",4)
  val X3 = Variable("X3",2)
  val X4 = Variable("X4",2)

   // Se crea asignacion , dando valores 2, 3, 1 y 0 a las variables
   val asignacion1=Asignacion(Dominio(List(X1, X2, X3, X4)), List(2,3,1,0))
   println("Comprobacion vacio sobre asignacion no vacia: " + asignacion1.vacia)
   println("Se muestra la asignacion: ")
   println(asignacion1)

   // Calculo del indice asociado a la asignacion (debe ser 46)
   val indice1=asignacion1.calcularIndice
   println("indice1 (debe ser 46): " + indice1)

   // A partir del indice obtenemos la asignacion
   val asignacionDeIndice = Asignacion(asignacion1.dominio, indice1)

   // Se muestra la asignacion obtenida: debe ser X1=2, X2=3, X3=1, X4=0
   println("Asignacion resultante: " + asignacionDeIndice)

  val proyec = asignacion1.proyectar(Dominio(List(X4,X1)))

  println("res proyect" + proyec)

  val X5 = Variable("X5",8)
  val sum = proyec+(X2,0)

  println(sum)


}