case class NodoHoja(lvl:Int,val valor:Double) extends Nodo(lvl){


  def obtenerValores():List[Double] = List(valor)

  def obtenerNumeroHijos() = ???

  def asignarHijo(estado:Int,n:Nodo) = ???

  def obtenerVariable : Variable = ???

  def obtenerHijo(n:Int):Nodo = this

  def obtenerValor(a:Asignacion) : Double = valor

  def combinar(otro:Nodo):Nodo = {
    otro match {
      case NodoHoja(a,b)=> {
        NodoHoja(this.nivel,this.valor*otro.obtenerValores().head)
      }
      case NodoVariable(a,b)=>{
        otro.combinar(this)
      }
    }
  }

  def restringir(v:Variable = null,valor:Int):Nodo = {
    NodoHoja(this.nivel,this.valor)
  }
}

object NodoHoja {

  def apply(lvl: Int, v: Double): NodoHoja = new NodoHoja(lvl, v)
}
