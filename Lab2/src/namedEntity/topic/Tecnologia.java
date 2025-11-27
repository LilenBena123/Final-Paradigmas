package namedEntity.topic;

public class Tecnologia extends Topic{
    String rama;

    public Tecnologia(String name){
        super();
        this.name = name;
    }

    public void setTecnologiaRama(String campo){
        this.rama = campo;
    }

    public String getTecnologiaRama(){
        return this.rama;
    }

    public void prettyPrint(){
        System.out.println("name: " + getName() + " .\n");
    }
}