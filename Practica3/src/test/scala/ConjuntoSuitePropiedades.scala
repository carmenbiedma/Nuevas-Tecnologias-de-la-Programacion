
import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, throws, AnyOperators}
import org.scalacheck.Gen._

object ConjuntoSuitePropiedades extends Properties("Test sobre conjunto") {
   val valor = choose(0, 10)

   /**
     * Generacion de secuencia de tamaño
     *
     * @param tam
     * @return
     */
   def secuencia(tam: Int): Range = {
      val inicio = valor.sample.getOrElse(0)
      inicio to (inicio + tam)
   }

   /**
     * Propiedad para probar el metodo de obtencion de la longitud
     */
   property("conjunto de tamaño uno") =
      forAll(valor) {
         valor => {
            // Se crea el conjunto de un elemento
            val conjunto = Conjunto.conjuntoUnElemento(valor)

            // Se comprueba que el conjunto contiene el valor
            conjunto(valor) == true
         }
      }

   property("conjunto union") =
      forAll(valor) {
         valor => {
            val secuencia1 = secuencia(10)
            val secuencia2 = secuencia(10)

            // Se generan los conjuntos a unir
            val conjunto1 = Conjunto(x => x >= secuencia1.min && x <= secuencia1.max)
            val conjunto2 = Conjunto(x => x >= secuencia2.min && x <= secuencia2.max)

            // Se produce la union
            val union = Conjunto.union(conjunto1, conjunto2)

            // Se itera sobre la union de ambos rangos y se comprueba la
            // pertenencia al conjunto
            val rangoUnion = secuencia1.toList ::: secuencia2.toList

            // De cumplirse que cada elemento esta en el conjunto union
            val resultado = rangoUnion.map(valor => {
               union(valor) == true
            })

            val global: Boolean = resultado.forall(res => res == true)
            global == true
         }
      }


  property("conjunto interseccion") =
    forAll(valor) {
      valor => {
        val secuencia1 = secuencia(10)
        val secuencia2 = secuencia(10)


        val conjunto1 = Conjunto(x => x >= secuencia1.min && x <= secuencia1.max)
        val conjunto2 = Conjunto(x => x >= secuencia2.min && x <= secuencia2.max)


        val interseccion = Conjunto.interseccion(conjunto1, conjunto2)


        val rangoInterseccion = secuencia1.toList.intersect(secuencia2.toList)

        // De cumplirse que cada elemento esta en el conjunto union
        val resultado = rangoInterseccion.map(valor => {
          interseccion(valor) == true
        })

        val global: Boolean = resultado.forall(res => res == true)
        global == true
      }
    }

  property("conjunto diferencia") =
    forAll(valor) {
      valor => {
        val secuencia1 = secuencia(10)
        val secuencia2 = secuencia(10)


        val conjunto1 = Conjunto(x => x >= secuencia1.min && x <= secuencia1.max)
        val conjunto2 = Conjunto(x => x >= secuencia2.min && x <= secuencia2.max)


        val diferencia = Conjunto.diferencia(conjunto1, conjunto2)

        val rangoDiferencia = secuencia1.toList.diff(secuencia2.toList)

        val resultado = rangoDiferencia.map(valor => {
          diferencia(valor) == true
        })

        val global: Boolean = resultado.forall(res => res == true)
        global == true
      }
    }

  property("filtrar") =
      forAll(valor) {
        valor => {
          val secuencia1 = secuencia(10)

          // Se generan los conjuntos a unir
          val conjunto1 = Conjunto(x => x >= secuencia1.min && x <= secuencia1.max)

          // Se produce la union
          val filtrado = Conjunto.filtrar(conjunto1,(x:Int) => x > 3)

          val rangoFiltrado = secuencia1.toList.filter((x:Int) => x > 3)

          val resultado = rangoFiltrado.map(valor => {
            filtrado(valor) == true
          })

          val global: Boolean = resultado.forall(res => res == true)
          global == true
        }
      }

  property("Para todo") =
    forAll(valor) {
      valor => {
        val secuencia1 = secuencia(10)

        // Se generan los conjuntos a unir
        val conjunto1 = Conjunto(x => x >= secuencia1.min && x <= secuencia1.max)

        // Se produce la union
        val paraTodoConjunto = Conjunto.paraTodo(conjunto1,(x:Int) => x > 3)


        val paraTodoList = secuencia1.toList.forall((x:Int) => x > 3)

        paraTodoConjunto == paraTodoList
      }
    }

  property("Existe") =
    forAll(valor) {
      valor => {
        val secuencia1 = secuencia(10)

        // Se generan los conjuntos a unir
        val conjunto1 = Conjunto(x => x >= secuencia1.min && x <= secuencia1.max)

        // Se produce la union
        val existeConjunto = Conjunto.existe(conjunto1,(x:Int) => x > 3)

        val existeList = secuencia1.toList.exists((x:Int) => x > 3)

        existeConjunto == existeList
      }
    }

  property("map") =
    forAll(valor) {
      valor => {
        val secuencia1 = secuencia(10)

        val conjunto1 = Conjunto(x => x >= secuencia1.min && x <= secuencia1.max)

        val mapConjunto = Conjunto.map(conjunto1, (x:Int) => x*2)


        val mapList = secuencia1.toList.map((x:Int) => x*2)
        val resultado = mapList.map(valor => {
          mapConjunto(valor) == true
        })

        val global: Boolean = resultado.forall(res => res == true)
        global == true
      }
    }



}