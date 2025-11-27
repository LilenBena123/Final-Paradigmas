package Lab2.ejerciciosParcial;


public class Persona extends EntidadNombrada {
    String nombre;
    String apellido;

    public String getNombre(){
    return this.nombre;
    }
    public void setNombre(String nombre){
        this.nombre=nombre;
    }
    public String getApellido(){
    return this.apellido;
    }
    public void setApellido(String apellido){
        this.apellido=apellido;
    }

    @Override    
    public String getTipo(){
    return "Persona";
    }

}
