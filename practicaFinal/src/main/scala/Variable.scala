class Variable(val nombre:String, val n_estados:Int){

  override def toString: String = {
    "En clase Variable: Nombre = "+this.nombre+"  numero estados = "+n_estados
  }

  /**
    * sobrecarga del operador ==
    * @param v : Variable a comparar conm this.
    * @return true si son iguales, false si son distintas
    */
  def == (v:Variable): Boolean ={
    if(nombre == v.nombre && n_estados == v.n_estados) true
    else false
  }

}

object Variable {

  def apply(n : String , s : Int): Variable = {
    new Variable (n,s)
  }

}

object pruebaVariable extends App {

  val X1 = Variable ("X1", 2)

  println(X1)
}