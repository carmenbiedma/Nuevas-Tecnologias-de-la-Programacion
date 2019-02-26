case class ValoresArbol(val d:Dominio,val raiz:Nodo) extends Valores(d){

  override def toString: String = {
    val espacio=" "
    def go (index:Int,n:Nodo):String = {
      n match {
        case NodoVariable(l,v) => {
          (0 until n.obtenerNumeroHijos).map(estado => {
            espacio*l + v.nombre  + " : " + estado + "\n" + go(index+1,n.obtenerHijo(estado))

          }).mkString("")
        }
        case NodoHoja(l,value) => (" = " + value + "\n").toString
      }
    }
    "En clase ValoresArbol" + go(0,raiz)
  }


  def obtenerValor(a:Asignacion):Double = {

    def go(n:Nodo) : Double = {
      n match {
        case NodoVariable(l,v) => go(n.obtenerHijo(a.datos(v)))
        case NodoHoja(l,valor) => valor
      }
    }

    go(raiz)
  }
  def obtenerValores():List[Double] = {

    var l = List[Double]()

    def go (n:Nodo):List[Double]= {
      n match {
        case NodoHoja(lvl, value) => {
          l++=List(value)
          l
        }
        case NodoVariable(lvl, v) => {
          (0 until v.n_estados).map(estado => {
            go(n.obtenerHijo(estado))
          })
          l
        }
      }

    }
    go(raiz)

  }
  def obtenerVariables():List[Variable] = {
    var l = List[Variable]()

    def go(n:Nodo) : List[Variable] = {
     n match {
       case NodoVariable(lvl,v) => {
         l++=List(v)
         go(n.obtenerHijo(0))
         l
       }
       case NodoHoja(lvl,v) => l:::List()
     }
    }

    go(raiz)
  }
  def combinar(otro:Valores):Valores = {
    otro match{
      case ValoresArray(dominio,valores) => this.combinarArbolArbol(otro.convertir)
      case ValoresArbol(dominio,raiz) => this.combinarArbolArbol(otro)
    }
  }

  def combinarArbolArbol(otro:Valores):Valores= {
    val dominioFinal=this.dominio+otro.dominio
    ValoresArbol(dominioFinal, raiz.combinar(otro.raiz()))
  }

  def restringir(variable:Variable,valor:Int):Valores= {
    val dominioFinal=dominio-variable
    ValoresArbol(dominioFinal, this.raiz.restringir(variable,valor))
  }

  def convertir:Valores= {
    ValoresArray(d,obtenerValores())
  }

}

object ValoresArbol {

  def apply(d: Dominio, v:List[Double]): ValoresArbol = {

  def go(index : Int,asignacion: Asignacion): Nodo ={

    val variable = d.variables(index)

    val nodo = NodoVariable(index,variable)

    if (index < d.longitud - 1) {
      (0 until variable.n_estados).foreach( i => {
        nodo.asignarHijo(i,go(index+1,asignacion+(variable,i)))
      })

    }
    else{
      (0 until variable.n_estados).foreach(i => {
        val asignacionHoja = asignacion + (variable,i)
        val hoja = NodoHoja(index,v(asignacionHoja.calcularIndice))

        nodo.asignarHijo(i,hoja)
      })
    }
    nodo
  }
  val raiz = go(0,Asignacion(Dominio(List())))

  new ValoresArbol(d,raiz)

}



}

object pruebaValoresArbol extends App{

  val X1 = Variable("X1",2)
  val X2 = Variable("X2",2)
  val X3 = Variable("X3",2)

  // Se crea un dominio con las variables creadas antes
  val dominioNoVacio=Dominio(List(X1,X2))

  val arbol = ValoresArbol(dominioNoVacio,List(0.24,0.16,0.36,0.24))

  println(arbol)

  println(arbol.obtenerValor(Asignacion(dominioNoVacio,List(1,1))))

  println("Lista de valores" + arbol.obtenerValores())

  println("Lista de variables" + arbol.obtenerVariables())

 println("COMBINAR:__________________________")



  val primero = ValoresArbol(dominioNoVacio,List(0.3,0.7,0.6,0.4))
  val dominio2 = Dominio(List(X1,X3))
  val segundo = ValoresArbol(dominio2,List(0.1,0.1,1,0))

  val combinado = primero.combinar(segundo)
  println(primero)
  println("______________")
  println(segundo)
  println("______________")
  println(combinado)

  val tercero = ValoresArbol(Dominio(List(X1,X2,X3)),List(0.27,0.03,0.63,0.07,0.6,0.0,0.4,0.0))
  println("Restringir" + tercero.restringir(X2,0))

}

