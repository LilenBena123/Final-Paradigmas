package namedEntity.topic;

public class Formula1 extends Deportes{
    String escuderia;
    String piloto;

    public Formula1(String name){
        super(name);
    }

    public void setFormula1Escuderia(String name){
        this.escuderia = name;
    }

    public void setFormula1Piloto(String name){
        this.piloto = name;
    }

    public String getFromula1Escuderia(){
        return this.escuderia;
    }

    public String getFromula1Piloto(){
        return this.piloto;
    }
}