package namedEntity.topic;

public class Basket extends Deportes{
    String nombre_equipo;
    String confederacion;

    public Basket(String name){
        super(name);
    }

    public void setBasketNombreEquipo(String name){
        this.nombre_equipo = name;
    }

    public void setBasketConfederacion(String confederacion){
        this.confederacion = confederacion;
    }

    public String getBasketNombreEquipo(){
        return this.nombre_equipo;
    }

    public String getBasketConfederacion(){
        return this.confederacion;
    }
} 