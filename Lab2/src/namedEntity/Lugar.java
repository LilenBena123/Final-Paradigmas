package namedEntity;

import namedEntity.topic.Topic;

public class Lugar extends NamedEntity{
	String pais;
	String ciudad;
	int direccion;

	public Lugar(String name, int frequency){
		super(name, "Lugar", frequency);
	}

	public void setLugarPais(String pais){
		this.pais = pais;
	}

	public void setLugarCiudad(String ciudad){
		this.ciudad = ciudad;
	}

	public void setLugarDireccion(int direccion){
		this.direccion = direccion;
	}

	public String getLugarPais(){
		return this.pais;
	}

	public String getLugarCiudad(){
		return this.ciudad;
	}

	public int getLugarDireccion(){
		return this.direccion;
	}

	@Override
	public String toString() {
    	return "Lugar [pais=" + pais +
           	", ciudad=" + ciudad +
           	", direccion=" + direccion + "]";
	}

	@Override
    public void prettyPrint() {
	    this.pais = this.pais == null ? "" : this.pais;
	    this.ciudad= this.ciudad== null ? "" : this.ciudad;
	    System.out.println(
			    getName() + " " +
			    pais + " " +
			    ciudad + " " +
			    direccion + " " +
			    getFrequency() + " " +
				getTopic()
			    );
    }
}
