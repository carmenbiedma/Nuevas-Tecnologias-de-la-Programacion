
/**
  * Interfaz generica para una lista
  */

  sealed trait Lista[+A]

/**
  * Declaracion de lista vacia
  */

  case object Nil extends  Lista[Nothing]

/**
  * Lista con elementos
  */

  case class Cons[+A](cabeza : A, cola: Lista[A]) extends Lista[A]
  object Lista{
    //A* = Lista variable de argumentos
    def apply[A](elementos: A*) : Lista[A] = {
      if(elementos.isEmpty) Nil
      else Cons(elementos.head,apply(elementos.tail : _*))
    }
  def longitud[A](lista: Lista[A]):Int = {
      lista match {
        case Nil => 0
        case Cons(head,tail) => 1+(longitud(tail))
      }
    }
    def sumaEnteros(enteros : Lista[Int] ) : Double = enteros match {
      case Nil => 0.0
      case Cons(head,tail) => head.toDouble + sumaEnteros(tail)
    }


  }





  object prueba extends App{
    val lista1 = Lista(1,2,3,4)
    //Si no tuviesemos el metodo que hemos hehco habria qur hacer
    val lista2= Cons(1,Cons(2,Cons(3,Cons(4,Nil))))
  }
