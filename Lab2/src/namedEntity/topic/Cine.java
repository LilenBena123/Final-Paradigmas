package namedEntity.topic;

public class Cine extends Cultura{
    String director;
    String nombre_pelicula;
    int recaudacion;

    public Cine(String name){
        super(name);
    }

    public void setCineDirector(String name){
        this.director = name;
    }

    public void setCineNombrePelicula(String name){
        this.nombre_pelicula = name;
    }

    public void setCineRecaudacion(int reucadacion){
        this.recaudacion = recaudacion;
    }

    public int getRecaudacion(){
        return this.recaudacion;
    }

    public String getDirector(){
        return this.director;
    }

    public String getNombrePelicula(){
        return this.nombre_pelicula;
    }
}