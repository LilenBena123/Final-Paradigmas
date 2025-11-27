package namedEntity.topic;

public class Politica extends Topic{
    String pais;
    String cargo;

    public Politica(String name){
        super();
        this.name = name;
    }

    public void setPoliticaPais(String pais){
        this.pais = pais;
    }

    public void setPoliticaCargo(String cargo){
        this.cargo = cargo;
    }

    public String getPoliticaPais(){
        return this.pais;
    }

    public String getPoliticaCargo(){
        return this.cargo;
    }

    public void prettyPrint(){
        System.out.println("name: " + getName() + "");
    }
}