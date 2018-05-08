import com.sun.java.swing.plaf.windows.WindowsTreeUI;

import java.io.IOException;
import java.lang.reflect.Array;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.cert.CollectionCertStoreParameters;
import java.util.*;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.LongStream;
import java.util.stream.Stream;

public class ListadoEmpleados {

    private List<Empleado> listadoArchivo;
    private Map<String,Empleado> listado;
    private static Pattern patronEspacios=Pattern.compile("\\s+");

    ListadoEmpleados(String archivo) throws IOException {

        listado = new HashMap<>(); //Creamos el diccionario de empleado
        listadoArchivo = new LinkedList<>();

        Stream<String> lineas = Files.lines(Paths.get(archivo), StandardCharsets.ISO_8859_1);

        lineas.forEach( linea -> {
            Empleado e = crearEmpleado(linea);
            listadoArchivo.add(e);
        });

    }

    private Empleado crearEmpleado(String linea){

        Pattern patron = Pattern.compile(",");
        List<String> elementos = Arrays.asList(patron.split(linea));
        Empleado empleado = new Empleado(elementos.get(0),elementos.get(1),elementos.get(2),elementos.get(3));
        return empleado;
    }

    public void imprimirListadoArchivo(){
        listadoArchivo.stream().forEach(System.out::println);
    }

    public int obtenerNumeroEmpleadosArchivo(){

        return listadoArchivo.size();
    }

    public boolean hayDnisRepetidosArchivo(){

        Stream<String> dnis = listadoArchivo.stream().map(Empleado::obtenerDni);
        List<String> sinRepetir = dnis.distinct().collect(Collectors.toList());

        if(sinRepetir.size() < listadoArchivo.size())
            return true;

        return false;
    }


    public Map<String, List<Empleado>> obtenerDnisRepetidosArchivo(){
        Map<String,List<Empleado>> listaProcesada = listadoArchivo.stream().collect(Collectors.groupingBy(Empleado::obtenerDni)); //Agrupamos por dni a los empleados para ver cuales estan repetidos

        return listaProcesada.entrySet().stream().filter(e -> e.getValue().size() > 1).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, (key, e) -> e));

    }

    public int contarEmpleadosDnisRepetidos(){

        Stream<Map.Entry<String, List<Empleado>>> repes = obtenerDnisRepetidosArchivo().entrySet().stream();
        return repes.mapToInt(elemento -> elemento.getValue().size()).sum(); //Convertimos cada elemento del diccionario en un entero con el tamaño de la lista

    }

    public void repararDnisRepetidos(Map<String, List<Empleado>> repetidos){

        Stream<Map.Entry<String, List<Empleado>>> r = repetidos.entrySet().stream();
        r.forEach( elemento -> {
            elemento.getValue().stream().forEach(empleado -> {
                listadoArchivo.get(listadoArchivo.indexOf(empleado)).asignarDniAleatorio();
            });
        });
    }

    public boolean hayCorreosRepetidosArchivo(){

        Stream<String> correos = listadoArchivo.stream().map(Empleado::obtenerCorreo);
        List<String> sinRepetir = correos.distinct().collect(Collectors.toList());

        if(sinRepetir.size() < listadoArchivo.size())
            return true;

        return false;
    }

    public Map<String, List<Empleado>> obtenerCorreosRepetidosArchivo(){

        Map<String,List<Empleado>> listaProcesada = listadoArchivo.stream().collect(Collectors.groupingBy(Empleado::obtenerCorreo));

        return listaProcesada.entrySet().stream().filter(e -> e.getValue().size() > 1).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, (key, e) -> e));

    }

    public int contarEmpleadosCorreosRepetidos(){

        Stream<Map.Entry<String, List<Empleado>>> repes = obtenerCorreosRepetidosArchivo().entrySet().stream();
        return repes.mapToInt(elemento -> elemento.getValue().size()).sum(); //Convertimos cada elemento del diccionario en un entero con el tamaño de la lista

    }

    public void repararCorreosRepetidos(Map<String, List<Empleado>> repetidos){

        Stream<Map.Entry<String, List<Empleado>>> r = repetidos.entrySet().stream();
        r.forEach( elemento -> {
            elemento.getValue().stream().forEach(empleado -> {
                listadoArchivo.get(listadoArchivo.indexOf(empleado)).generarCorreoCompleto();
            });
        });
    }

    public void validarListaArchivo(){

        listadoArchivo.stream().forEach( empleado -> listado.put(empleado.obtenerDni(),empleado));

    }

    //___________________________________SEGUNDA PARTE_________________________________________

    public long cargarArchivoAsignacionSector(String nombreArchivo) throws IOException {

        List<String> lineas = Files.lines(Paths.get(nombreArchivo)).collect(Collectors.toList());
        Sector sector = procesarNombreSector(lineas.get(0));

        return lineas.stream().skip(2).map(linea -> procesarAsignacionSector(sector,linea)).filter(flag -> flag == false).count();

    }

    public  Sector procesarNombreSector(String cadena){

        List<String> infos = patronEspacios.splitAsStream(cadena).collect(Collectors.toList());
        Predicate<Sector> condicion = sector -> (sector.name().equals(infos.get(0)));
        return Arrays.stream(Sector.values()).filter(condicion).findFirst().get();

    }

    public boolean procesarAsignacionSector(Sector s, String dni){

        List<String> infos = patronEspacios.splitAsStream(dni).collect(Collectors.toList());
        Empleado empleado = listado.get(infos.get(0));

        if(empleado == null ) return false;

        empleado.asignarSector(s);
        return true;
    }

    public long cargarArchivoAsignacionRuta(String archivo) throws IOException {

        List<String> lineas = Files.lines(Paths.get(archivo)).collect(Collectors.toList());
       Ruta ruta = procesarNombreRuta(lineas.get(0));

        return lineas.stream().skip(2).map(linea -> procesarAsignacionRuta(ruta,linea)).filter(flag -> flag == false).count();

    }


    public  Ruta procesarNombreRuta(String cadena){

        List<String> infos = patronEspacios.splitAsStream(cadena).collect(Collectors.toList());
        Predicate<Ruta> condicion = ruta -> (ruta.name().equals(infos.get(0)));
        return Arrays.stream(Ruta.values()).filter(condicion).findFirst().get();

    }

    public boolean procesarAsignacionRuta(Ruta r,String dni){


        List<String> infos = patronEspacios.splitAsStream(dni).collect(Collectors.toList());
        Empleado empleado = listado.get(infos.get(0));

        if(empleado == null ) return false;

        empleado.asignarRuta(r);
        return true;
    }

    public Map<Ruta, Long> obtenerContadoresRuta(Sector sector){

        return listado.entrySet().stream().
                filter( e -> e.getValue().obtenerSector().equals(sector)).
                collect(Collectors.groupingBy(e -> e.getValue().obtenerRuta(),TreeMap::new,Collectors.counting()));
    }

    public Map<Sector, Map<Ruta, Long>> obtenerContadoresSectorRuta(){
        Map<Sector,Map<Ruta,Long>> result = new TreeMap<>();
        Arrays.stream(Sector.values()).forEach( s -> result.put(s,obtenerContadoresRuta(s)));
        result.entrySet().stream().forEach(System.out::println);
        return result;
    }

    public long[] obtenerContadoresSectores(){

        long a[];
        Map<Sector, Map<Ruta, Long>> sectorRuta = obtenerContadoresSectorRuta();
        return sectorRuta.entrySet().stream().mapToLong(e -> e.getValue().entrySet().stream().mapToLong(c -> c.getValue()).sum()).toArray();
    }

    public List <Empleado> buscarEmpleadosSinSectorSinRuta(){

        return listado.values().stream().filter(e -> e.obtenerSector() == Sector.NOSECTOR || e.obtenerRuta() == Ruta.NORUTA).collect(Collectors.toList());

    }

    public List <Empleado> buscarEmpleadosSinRuta(Sector s){

        return listado.values().stream().filter(e -> e.obtenerSector() == s && e.obtenerRuta() == Ruta.NORUTA).collect(Collectors.toList());
    }

    public List <Empleado> buscarEmpleadosConSectorSinRuta(){

        return listado.values().stream().filter(e -> e.obtenerSector() != Sector.NOSECTOR && e.obtenerRuta() == Ruta.NORUTA).collect(Collectors.toList());
    }

    List <Empleado> buscarEmpleadosSinSector(Ruta r){

        return listado.values().stream().filter(e -> e.obtenerSector() == Sector.NOSECTOR && e.obtenerRuta() == r).collect(Collectors.toList());
    }


    List <Empleado> buscarEmpleadosSinSectorConRuta(){

        return listado.values().stream().filter(e -> e.obtenerSector() == Sector.NOSECTOR && e.obtenerRuta() != Ruta.NORUTA).collect(Collectors.toList());
    }

    public static void main(String args[]) throws IOException{

        ListadoEmpleados listaprueba = new ListadoEmpleados("data/datos.txt");

        System.out.println(listaprueba.obtenerNumeroEmpleadosArchivo());

        System.out.println(listaprueba.hayDnisRepetidosArchivo());

        Map<String, List<Empleado>> r = listaprueba.obtenerDnisRepetidosArchivo();
        System.out.println(listaprueba.contarEmpleadosDnisRepetidos());
        listaprueba.repararDnisRepetidos(r);

        System.out.println("\nCOMPROBACION DESPUES DE REPARAR DNIS REPETIDOS\n");
        System.out.println(listaprueba.obtenerNumeroEmpleadosArchivo());
        System.out.println(listaprueba.hayDnisRepetidosArchivo());

        System.out.println("\n______________________________________________\n");
        System.out.println("\nPROCESAMIENTO DE CORREOS REPETIDOS\n");


        System.out.println(listaprueba.hayCorreosRepetidosArchivo());
        Map<String, List<Empleado>> r2 = listaprueba.obtenerCorreosRepetidosArchivo();
        System.out.println(listaprueba.contarEmpleadosCorreosRepetidos());
        listaprueba.repararCorreosRepetidos(r2);

        System.out.println("\nCOMPROBACION DESPUES DE REPARAR CORREOS REPETIDOS\n");
        System.out.println(listaprueba.obtenerNumeroEmpleadosArchivo());
        System.out.println(listaprueba.hayCorreosRepetidosArchivo());


        listaprueba.validarListaArchivo();

        //SEGUNDA PARTE
        System.out.println("\nSEGUNDA PARTE\n");
        System.out.println(listaprueba.cargarArchivoAsignacionSector("data/asignacionSECTOR1.txt"));
        listaprueba.cargarArchivoAsignacionSector("data/asignacionSECTOR2.txt");
        listaprueba.cargarArchivoAsignacionRuta("data/asignacionRUTA1.txt");
        listaprueba.cargarArchivoAsignacionRuta("data/asignacionRUTA2.txt");
        listaprueba.cargarArchivoAsignacionRuta("data/asignacionRUTA3.txt");

        Arrays.stream(listaprueba.obtenerContadoresSectores()).forEach(System.out::println);
        List<Empleado> p = listaprueba.buscarEmpleadosSinSectorSinRuta();
        System.out.println(p.size());

        System.out.println(listaprueba.buscarEmpleadosConSectorSinRuta().size());
        System.out.println(listaprueba.buscarEmpleadosSinSectorConRuta().size());


    }
}
