abstract class Potencial(val dominio:Dominio,val valores:Valores) {

   override def toString: String ={
     "En clase potencial :" + dominio.toString + "\n"
   }

  /**
    * Combina 2 potenciales
    * @param otro
    * @return
    */
  def combinar(otro:Potencial) : Potencial

  /**
    * Restringe un potencial al valor de una variable
    * @param v : Variable que restringe
    * @param estado : Estado que toma la variable
    * @return
    */
  def restringir(v:Variable,estado:Int) : Potencial

  /**
    * Devuelve todos los valores que contiene el potencial
    * @return
    */
  def obtenerValores():List[Double]

  /**
    * Convierte un potencial almacenado en una estructura de tipo arbol a
    * uno de esttructura de tipo array y viceversa.
    * @return
    */
  def convertir():Potencial

}
