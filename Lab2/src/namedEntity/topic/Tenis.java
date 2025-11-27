package namedEntity.topic;

public class Tenis extends Deportes{
    String nombre_tenista;

    public Tenis(String name){
        super(name);
    }

    public void setTenisNombreTenista(String name){
        this.nombre_tenista = name;
    }

    public String setTenisNombreTenista(){
        return this.nombre_tenista;
    }
}