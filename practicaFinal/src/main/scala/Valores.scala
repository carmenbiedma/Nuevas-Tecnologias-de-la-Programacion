abstract class Valores(val dominio:Dominio) {

  override def toString: String = {
    "En clase Valores: "+ dominio.toString+"\n"
  }

  /**
    * Devuelve la raiz de un Arbol (no implementado para el caso Array)
    * @return
    */

  def raiz():Nodo

  /**
    * Devuelve el valor asociado a una asignación complta
    * @param a : Asignación de la cual queremos saber el valor
    * @return Valor de la asignación correspondiente
    */

  def obtenerValor(a:Asignacion):Double

  /**
    * Devuelve todos los valores de todas las asignaciones posibles
    * @return
    */

  def obtenerValores():List[Double]

  /**
    * Devuelve las variables que forman el dominio asociado a la clase
    * @return
    */

  def obtenerVariables():List[Variable]

  /**
    * Realiza la combinación de dos objetos de la clase Valor.
    * @param v : Objeto de la clase Valores que combinaremos con this
    * @return El resultado será un nuevo objeto
    */
  def combinar(v:Valores):Valores

  /**
    * Se restringe el conjunto de valores a una variable y un estado concreto
    * @param v
    * @param value
    * @return Devuelve un nuevo objeto quitando la dependencia de this con la variable v
    *         estableciendola a un estado fijo
    */
  def restringir(v:Variable,value:Int):Valores

  /**
    * Convierte un objeto específico de la clase Array a clase Arbol y viceversa, depende
    * de donde se esté ejecutando el método.
    * @return
    */
  def convertir:Valores

}
