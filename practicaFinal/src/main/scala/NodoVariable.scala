case class NodoVariable(val lvl:Int,val variable:Variable) extends Nodo(lvl){

  var hijos = Map[Int,Nodo]() //con tantos valores como estadps trnga la variable

  def obtenerValores() : List[Double] = {
    var lista = List[Double]()
    def go(n:Nodo):List[Double] = {
      n match {
        case NodoHoja(a, b) => lista++=List[Double](b)
        case NodoVariable(a, b) => (0 until n.obtenerNumeroHijos()).map(i => go(n.obtenerHijo(i)))
      }
      lista
    }
    go(this)
  }

  def asignarHijo(estado:Int,n:Nodo):Unit = hijos++=Map(estado->n)

  def obtenerVariable():Variable = variable
  def obtenerHijo(n:Int):Nodo = hijos(n)


  def obtenerNumeroHijos():Int = hijos.size

  def obtenerValor(asig:Asignacion):Double = {
    def go(n:Nodo,indice:Int):Double={
      n match {
        case NodoHoja(a,b)=>{
          n.obtenerValores().head
        }
        case NodoVariable(a,b)=>{
          go(n.obtenerHijo(asig.valores(indice)),indice+1)
        }
      }
    }
    go(this,0)
  }

  def combinar(otro:Nodo):Nodo = {
    val nuevo=NodoVariable(nivel,variable)
    otro match {
      case NodoHoja(lvl, variable) => {
        def go(n: Nodo, nuevo: Nodo, estado: Int): Nodo = {
          n match {
            case NodoHoja(lvl, valor) => {
              nuevo.asignarHijo(estado, n.combinar(otro))
              nuevo
            }
            case NodoVariable(lvl, variable) => {
              (0 until n.obtenerNumeroHijos()).map(i => {
                n.obtenerHijo(i) match {
                  case NodoVariable(lvl,variable)=>{
                     nuevo.asignarHijo(i,n.obtenerHijo(i))
                     go(n.obtenerHijo(i), nuevo.obtenerHijo(i), i)
                  }
                  case NodoHoja(lvl,variale) => go(n.obtenerHijo(i), nuevo, i)
                }
              })
              nuevo
            }
          }
        }
        go(this,NodoVariable(this.nivel,this.variable), 0)
      }
      case NodoVariable(level,variable2)=>{
        (0 until variable.n_estados).map(indice=> {
          println(otro.obtenerHijo(indice))
          val otroFinal = otro.restringir(variable,indice)
          nuevo.asignarHijo(indice,obtenerHijo(indice).combinar(otroFinal))
        })
        nuevo
      }


    }
  }

  def restringir(v:Variable,estado:Int): Nodo = {

    if (variable == v) {
      obtenerHijo(estado)
    }
    else {
      val nodoResultado = NodoVariable(this.nivel, this.variable)
      (0 until variable.n_estados).map(i => {
        nodoResultado.asignarHijo(i, obtenerHijo(i).restringir(v, estado))
      })
      nodoResultado
    }
  }
}

object NodoVariable {

  def apply(lvl: Int,v:Variable): NodoVariable = new NodoVariable(lvl,v)

}

object pruebaNodoVariable extends App{

  val X1 = Variable("X1",2)
  val X2 = Variable("X2",2)
  val X3 = Variable("X3",2)
  val nodoHoja1=NodoHoja(1,0.3)

  val n = NodoVariable(0,X1)
  n.asignarHijo(0,NodoVariable(1,X2))
  n.asignarHijo(1,NodoVariable(1,X2))

  n.obtenerHijo(0).asignarHijo(0,NodoHoja(2,0.3))
  n.obtenerHijo(0).asignarHijo(1,NodoHoja(2,0.7))
  n.obtenerHijo(1).asignarHijo(0,NodoHoja(2,0.6))
  n.obtenerHijo(1).asignarHijo(1,NodoHoja(2,0.4))


  print("X2" + n.obtenerHijo(0))

  println("Valores" + n.obtenerValores())

  val comb = n.combinar(nodoHoja1)

  println("COMBINADO " + comb.obtenerValores())

  println(n.obtenerHijo(1).obtenerValores())



  //val nodo = nodoVariable.combinar(nodohoja)

//  println(nodo+ "  " + nodo.obtenerHijo(0) + "  " + nodo.obtenerHijo(1))

 // println("__________________________________")


 // val nodoVariable2 = NodoVariable(0,X2)
 // nodoVariable2.asignarHijo(0,NodoHoja(1,0.4))
 // nodoVariable2.asignarHijo(1,NodoHoja(1,0.6))

  //val nodo2 = nodoVariable.combinar(nodoVariable2)
  //println(nodo2.obtenerHijo(0).obtenerHijo(0) + "  " + nodoVariable2.obtenerHijo(0).obtenerHijo(1) +
   // "  " + nodo2.obtenerHijo(1).obtenerHijo(0) + " " + nodo2.obtenerHijo(1).obtenerHijo(1))

  //println(nodoVariable.obtenerValores())


  val n2 = NodoVariable(0,X1)
  n2.asignarHijo(0,NodoVariable(1,X2))
  n2.asignarHijo(1,NodoVariable(1,X2))

  n2.obtenerHijo(0).asignarHijo(0,NodoVariable(2,X3))
  n2.obtenerHijo(0).asignarHijo(1,NodoVariable(2,X3))
  n2.obtenerHijo(1).asignarHijo(0,NodoVariable(2,X3))
  n2.obtenerHijo(1).asignarHijo(1,NodoVariable(2,X3))

  n2.obtenerHijo(0).obtenerHijo(0).asignarHijo(0,NodoHoja(3,0.27))
  n2.obtenerHijo(0).obtenerHijo(0).asignarHijo(1,NodoHoja(3,0.03))
  n2.obtenerHijo(0).obtenerHijo(1).asignarHijo(0,NodoHoja(3,0.63))
  n2.obtenerHijo(0).obtenerHijo(1).asignarHijo(1,NodoHoja(3,0.07))
  n2.obtenerHijo(1).obtenerHijo(0).asignarHijo(0,NodoHoja(3,0.6))
  n2.obtenerHijo(1).obtenerHijo(0).asignarHijo(1,NodoHoja(3,0.0))
  n2.obtenerHijo(1).obtenerHijo(1).asignarHijo(0,NodoHoja(3,0.4))
  n2.obtenerHijo(1).obtenerHijo(1).asignarHijo(1,NodoHoja(3,0.0))

  println("RESTRINGIR = " + n2.restringir(X3,0).obtenerValores())



}

