package namedEntity.topic;

public class Futbol extends Deportes{
    String nombre_equipo;

    public Futbol(String name){
        super(name);
    }

    public void setFutbolNombreEquipo(String name){
        this.nombre_equipo = name;
    }

    public String getFutbolNombreEquipo(){
        return this.nombre_equipo;
    }
}