package namedEntity;

import namedEntity.topic.Topic;

public class Organization extends NamedEntity{
    int ingresos;

    public Organization(String name, int frequency){
        super(name, "Organizacion", frequency);
    }

    public void setOrganizacionIngresos(int ingresos){
        this.ingresos = ingresos;
    }

    public int getOrganizacionIngresos(){
        return this.ingresos;
    }

    @Override
    public String toString(){
        return "Organizacion [name=" + getName() + 
        ", ingresos= " + ingresos + 
        "]";
    }

    @Override
    public void prettyPrint(){
	    System.out.println(
			    getName() + " " +
			    ingresos + " " +
			    getFrequency() + " " +
                getTopic()
			    );
    }
}
