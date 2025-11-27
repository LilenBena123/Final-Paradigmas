package parser;
import java.io.File;

/*Esta clase modela los atributos y metodos comunes a todos los distintos tipos de parser existentes en la aplicacion*/
public abstract class GeneralParser {
	public static boolean fileExists(String path){
		File file = new File(path);
		return file.exists();

	}

	public abstract feed.Feed parser(String arch);
	protected static final String TITLE= "title";
	protected static final String DESCRIPTION= "description";
	protected static final String PUBDATE= "pubDate";
	protected static final String LINK= "link";

}
