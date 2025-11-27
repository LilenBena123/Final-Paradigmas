package namedEntity.topic;

public class Nacional extends Politica{
    String provincia;

    public Nacional(String name){
        super(name);
    }

    public void setNacionalProvincia(String provincia){
        this.provincia = provincia;
    }

    public String getNacionalProvincia(){
        return this.provincia;
    }
}