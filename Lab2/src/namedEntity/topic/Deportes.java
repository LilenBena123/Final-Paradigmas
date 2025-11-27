package namedEntity.topic;

public class Deportes extends Topic{
    String elemento_de_juego;
    int n_jugadores;

    public Deportes(String name){
        super();
        this.name = name;
        
    }

    public void setDeportesN_Jugadores(int n_jugadores){
        this.n_jugadores = n_jugadores;
    }

    public void setDeportesElemento(String elemento){
        this.elemento_de_juego = elemento;
    }

    public int getDeportesN_Jugadores(){
        return this.n_jugadores;
    }

    public String getDeportesElemento(){
        return this.elemento_de_juego;
    }

    public void prettyPrint(){
        System.out.println("name: " + getName() + "");
    }

}