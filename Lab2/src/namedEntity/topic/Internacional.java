package namedEntity.topic;

public class Internacional extends Politica{
    String agencia;

    public Internacional(String name){
        super(name);
    }

    public void setInternacionalAgencia(String agencia){
        this.agencia = agencia;
    }

    public String getInternacionalAgencia(){
        return this.agencia;
    }
}