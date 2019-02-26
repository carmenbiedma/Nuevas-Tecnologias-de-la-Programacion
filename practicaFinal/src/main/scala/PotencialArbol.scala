class PotencialArbol  ( val dominioPT: Dominio ,val valoresPT : ValoresArbol) extends Potencial(dominioPT,valoresPT) {

  override def toString: String = {
    "En clase PotencialArbol" + super.toString + valores.toString
  }

  def combinar(otro:Potencial) : Potencial = {
    val combinado = valores.combinar(otro.valores)
    val dominioCombinado = combinado.dominio
    PotencialArbol(dominioCombinado,combinado.obtenerValores())
  }

  def restringir(v:Variable,estado:Int):Potencial = {
    val restringido = valores.restringir(v,estado)
    val dominioRestringido = restringido.dominio
    PotencialArbol(dominioRestringido,restringido.obtenerValores())
  }

  def obtenerValores:List[Double] = {
    valores.obtenerValores()
  }

  def convertir:Potencial = {
    PotencialArray(dominio,valores.obtenerValores())
  }


}

object PotencialArbol {

  def apply(d: Dominio, l:List[Double]): PotencialArbol = new PotencialArbol(d,ValoresArbol(d,l))

}
