package namedEntity.topic;

public class Cultura extends Topic{
    String estadio;

    public Cultura(String name){
        super();
        this.name = name;

    }

    public void setCulturaEstadio(String estadio){
        this.estadio = estadio;
    }

    public String getCulturaEstadio(){
        return this.estadio;
    }

    public void prettyPrint(){
        System.out.println("name: " + getName()+ "");
    }
}