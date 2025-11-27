package namedEntity.topic;

public class Musica extends Cultura{
    String banda;
    int seguidores;
    String genero;

    public Musica(String name){
        super(name);
    }

    public void setMusicaBanda(String name){
        this.banda = name;
    }

    public void setMusicaGenero(String name){
        this.genero = name;
    }

    public void setMusicaSeguidores(int seguidores){
        this.seguidores = seguidores;
    }

    public String getMusicaBanda(){
        return this.banda;
    }

    public String getMusicaGenero(){
        return this.genero;
    }

    public int getMusicaSeguidores(){
        return this.seguidores;
    }
}