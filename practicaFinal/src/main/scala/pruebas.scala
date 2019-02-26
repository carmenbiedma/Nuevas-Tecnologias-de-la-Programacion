object pruebas extends App {

  // Se crean 4 variables
  val X1 = Variable("X1",2)
  val X2 = Variable("X2",2)
  val X3 = Variable("X3",2)

  val dominio=Dominio(List(X1,X2,X3))
  val potencial1 = PotencialArray(dominio,List(0.27,0.03,0.63,0.07,0.6,0.0,0.4,0.0))

  println(potencial1)

  println("\n" + potencial1.convertir)

}
