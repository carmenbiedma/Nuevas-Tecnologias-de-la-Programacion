/**
  * Clase para representar conjuntos definidos mediante una funcion
  * caracteristica (un predicado). De esta forma, se declara el tipo
  * conjunto como un predicado que recibe un entero (elemento) como
  * argumento y dvuelve un valor booleano que indica si pertenece o no
  * al conjunto
  *
  * @param funcionCaracteristica
  */
 class Conjunto(val funcionCaracteristica: Int => Boolean) {
 /**
 * Crea una cadena con el contenido completo del conjunto
 *
 * @return
 */
 override def toString(): String = {
  // El uso de this(i) implica la el uso del metodo apply
   val elementos = for (i <- -Conjunto.LIMITE to Conjunto.LIMITE
                      if this(i)) yield i
   elementos.mkString("{", ",", "}")
 }

 /**
 * Metodo para determinar la pertenencia de un elemento al
 * conjunto
 * @param elemento
 * @return valor booleano indicando si elemento cumple
 * la funcion caracteristica o no
 */
 def apply(elemento: Int): Boolean = funcionCaracteristica(elemento)


}




/**
 * Objeto companion que ofrece metodos para trabajar con
 * conjuntos
 */
  object Conjunto {
  /**
   * Limite para la iteracion necesaria algunas operaciones,
   * entre -1000 y 1000
   */
   private var LIMITE = 1000

   /**
   * Metodo que permite crear objetos de la clase Conjunto
   * de forma sencilla
   * @param f
   * @return
   */
   def apply(f: Int => Boolean): Conjunto = {
     new Conjunto(f)
   }

  /**
    * Metodo que permite crear un conjunto con un elemento
    * @param elemento
    * @return
    */
  def conjuntoUnElemento(elemento : Int) : Conjunto = {
    new Conjunto ( (x:Int) => x==elemento )
  }

  /**
    * Metodo que devuelve la union de dos conjuntos
    * @param c1
    * @param c2
    * @return
    */
  def union(c1 : Conjunto, c2 : Conjunto) : Conjunto = {

    Conjunto( (x:Int) => c1(x) || c2(x) )

  }

  /**
    * MEtodo que devuelve la interseccion de dos conjuntos
    * @param c1
    * @param c2
    * @return
    */
  def interseccion(c1 : Conjunto, c2 : Conjunto) : Conjunto = {

    Conjunto ( (x:Int) => c1(x) && c2(x) )
  }

  /**
    * Metodo que devuelve la diferencia de c1 con c2
    * @param c1
    * @param c2
    * @return
    */
  def diferencia(c1 : Conjunto, c2 : Conjunto) : Conjunto = {

    Conjunto ( (x:Int) => c1(x)==true && c2(x)==false )

  }

  /**
    * Filtra el contenido de un conjunto de acuerdo con un predicado
    * @param c
    * @param predicado
    * @return
    */
  def filtrar(c : Conjunto, predicado : Int => Boolean) : Conjunto = {
    Conjunto ( (x:Int) => c(x) && predicado(x))
  }

  /**
    * Comprueba si para todos los elementos del conjunto se cumple predicado
    * @param conjunto
    * @param predicado
    * @return
    */
  def paraTodo(conjunto : Conjunto, predicado : Int => Boolean) : Boolean = {
    def iterar(elemento : Int) : Boolean = {
      if(elemento == LIMITE+1) true
      else if (conjunto(elemento) == false) iterar(elemento+1)
      else predicado(elemento) && iterar(elemento+1)
      }
    iterar(-LIMITE)
  }

  /**
    * Devuelve true si al menos un elemento cumple el predicado
    * @param c
    * @param predicado
    * @return
    */
  def existe(c : Conjunto, predicado : Int => Boolean) : Boolean = {

    var predicado2 = (x:Int) => {
      if(predicado(x)==true) false
      else true}

    if(paraTodo(c,predicado2)) false
    else true
  }

  /**
    * Tranforma los elementos del conjunto aplicando una funcion
    * @param c
    * @param funcion
    * @return
    */
  def map(c : Conjunto, funcion : Int => Int) : Conjunto = {
    //LIMITE=funcion(LIMITE) //Para que imprima correctamente todos los valores
    Conjunto((x:Int)=>existe(c,b=>funcion(b) == x))
  }


}

  object prueba extends App {

    // Creamos conjunto para valores mayores que 3
    val conjunto=new Conjunto((x:Int) => x > 3)

    // Se comprueba si 5 pertenece al conjunto
    val pertenece=conjunto(5)
    println(pertenece)

    val conjunto1 = new Conjunto((x:Int) => x<4)
    val conjunto2 = new Conjunto((x:Int) => x>0)

    val conjunto3 = Conjunto.map(conjunto1, (x:Int) => x*2)

    println(conjunto3)
  }
