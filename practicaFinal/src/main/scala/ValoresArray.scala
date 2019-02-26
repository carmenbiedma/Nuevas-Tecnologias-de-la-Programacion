case class ValoresArray(d:Dominio,val values:List[Double]) extends Valores(d){

  override def toString: String = {
    super.toString + "En clase ValoresArray" + (0 until dominio.maximoIndice).map(i => {
      Asignacion(dominio,i).toString + " = "+ values(i)
    }).mkString("\n")
  }
  def obtenerValor(a:Asignacion):Double = {
    values(a.calcularIndice)
  }
  def obtenerValores():List[Double] = {
    values
  }
  def obtenerVariables():List[Variable] = {
    dominio.variables
  }

  def raiz():Nodo = ???

  def combinar(otro:Valores):Valores =  {
    otro match {
      case ValoresArbol(lvl,r) => this.combinarArrayArray(otro.convertir)
      case ValoresArray(lvl,valores) => this.combinarArrayArray(otro)
    }
  }

  def combinarArrayArray(otro: Valores):Valores={
    var producto=List[Double]()
    val dominioFinal=this.dominio+otro.dominio
    (0 until dominioFinal.maximoIndice).map(x=> {
      val asignacionFinal=Asignacion(dominioFinal,x)
      val asignacionThis=asignacionFinal.proyectar(this.dominio)
      val asignacionOtro=asignacionFinal.proyectar(otro.dominio)
      producto++=List[Double](obtenerValor(asignacionThis)*otro.obtenerValor(asignacionOtro))
    })
    ValoresArray(dominioFinal,producto)
  }

  def restringir(variable:Variable,valor:Int):Valores= {
    val dominioFinal=dominio-variable
    val par=new Tuple2[Variable,Int](variable,valor)
    var valores=(0 until dominioFinal.maximoIndice).map(x=>{
      val asiginacionFinal=Asignacion(dominioFinal,x)
      val asignacionCompleta=asiginacionFinal+par
      val asignacionOrdenada = asignacionCompleta.proyectar(dominio)
      println("Asignacion completa ->" + asignacionCompleta)
      obtenerValor(asignacionOrdenada)
    }).toList
    ValoresArray(dominioFinal,valores)
  }

  def convertir:Valores = {
    ValoresArbol(dominio,values)
  }
}
object ValoresArray {
  def apply(d:Dominio,v:List[Double]) = new ValoresArray(d,v)
}
object pruebaValoresArray extends App {
  val X1 = Variable("X1",2)
  val X2 = Variable("X2",2)
  val X3 = Variable("X3",2)
  val dom_ejemplo = Dominio(List(X1,X2,X3))
  val valore_ejemplo = ValoresArray ( dom_ejemplo, List(0.2,0.8,0.6,0.1,0.2,0.2,0.3,0.3))
  println(valore_ejemplo)
  val a = Asignacion(dom_ejemplo,List(0,0,1))
  println("VALOR = " + valore_ejemplo.obtenerValor(a))
  val dom1 = Dominio(List(X1,X2))
  val valores1 = ValoresArray(dom1, List(0.24,0.16,0.36,0.24))
  val dom2 = Dominio(List(X3))
  val valores2 = ValoresArray(dom2,List(0.6,0.4))
  val valoresCombinacion = valores1.combinar(valores2)
  println("COmbinacion" + valoresCombinacion)

  val valoresRestringir = valore_ejemplo.restringir(X2,1)
  println("Restringir " + valoresRestringir )

}
