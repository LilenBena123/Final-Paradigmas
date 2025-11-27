package namedEntity;

import namedEntity.topic.Topic;
import namedEntity.topic.Futbol;

public class Person extends NamedEntity{
	String surname;
	String title;

	public Person(String name, int frequency){
		super(name, "Persona", frequency);
	}

	public void setPersonaSurname(String surname){
		this.surname = surname;
	}

	public void setPersonaTitle(String title){
		this.title = title;
	}

	public String getPersonaSurname(){
		return this.surname;
	}

	public String getPersonaTitle(){
		return this.title;
	}

	
    public String toString() {
        return "Persona [name=" + getName() +
               ", surname=" + surname +
               ", title="  + title +
               "]";
    }


    public void prettyPrint() {
	this.surname = this.surname == null ? "" : this.surname;    
	this.title = this.title == null ? "" : this.title;    
        System.out.println(getName() + " " + surname + " " + title + " " + getFrequency() + " " + getTopic());
    }

	public static void main (String[] args){

		Futbol futbolin = new Futbol("Fútbol");
		Person messi = new Person("Messi", 0);
		messi.setTopic(futbolin);

		messi.prettyPrint();

	}
}

