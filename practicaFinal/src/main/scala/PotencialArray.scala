class PotencialArray ( val dominioPA: Dominio , val valoresPA : ValoresArray) extends Potencial(dominioPA,valoresPA) {

  override def toString: String = {
    "En clase PotencialArray" + super.toString + valores.toString
  }

  def combinar(otro:Potencial) : Potencial = {
    val combinado = valores.combinar(otro.valores)
    val dominioCombinado = combinado.dominio
    PotencialArray(dominioCombinado,combinado.obtenerValores())
  }

  def restringir(v:Variable,estado:Int):Potencial = {
    val restringido = valores.restringir(v,estado)
    val dominioRestringido = restringido.dominio
    PotencialArray(dominioRestringido,restringido.obtenerValores())
  }

  def obtenerValores:List[Double] = {
    valores.obtenerValores()
  }

  def convertir:Potencial = {
    PotencialArbol(dominio,valores.obtenerValores())
  }

}

object PotencialArray {

  def apply(d: Dominio, l:List[Double]): PotencialArray = new PotencialArray(d, ValoresArray(d,l))

}
