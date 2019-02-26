abstract class Nodo(val nivel:Int) {

  /**
    * Devuelve los valores de los nodo hoja asociados a un Nodo (Su valor en caso de los nodos hoja)
    * @return Una lista con los valores
    */

  def obtenerValores():List[Double]

  /**
    * Devuelve la variable correspondiente a un nodo variable,
    * no hace nada en el caso de ser un nodo hoja
    * @return
    */

  def obtenerVariable: Variable

  /**
    * Devuelve el número de hijos que tiene un nodo variable,
    * no implementado en el caso de nodo hoja
    * @return
    */

  def obtenerNumeroHijos():Int

  /**
    * Asigna un hijo a un nodo variable,
    * no implementado en el caso de nodo hoja
    * @param estado : Estado de la variable del nodo actual al que asociara el hijo
    * @param n : Nodo que se asociará como hijo
    */

  def asignarHijo(estado:Int,n:Nodo): Unit

  /**
    * Devuelve el hijo en el estado n de un nodo.
    * En el caso de un nodo hoja se devuelve this
    * @param n: Estado del hijo que queremos obtener
    * @return
    */

  def obtenerHijo(n:Int):Nodo

  /**
    * Devuelve el valor del nodo hoja asignado al camino dado por la asignación pasada como argumento
    * @param a : ASignación de la cual obtendremos el valor asociado
    * @return
    */

  def obtenerValor(a:Asignacion) : Double

  /**
    * Combina 2 nodos
    * @param otro
    * @return
    */

  def combinar(otro:Nodo):Nodo

  /**
    * Restringe un nodo asignando como valor el argumento valor a la variable pasada como primer argumento
    * @param v : Variable que restringe
    * @param valor : Valor que toma la variable (estado)
    * @return Nuevo nodo con la restricción aplicada.
    */

  def restringir(v:Variable,valor:Int):Nodo


}
