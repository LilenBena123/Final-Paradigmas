package parser;

/* Esta clase implementa el parser de feed de tipo rss (xml)
 * https://www.tutorialspoint.com/java_xml/java_dom_parse_document.htm 
 * */


import org.w3c.dom.*;
import javax.xml.parsers.*;
import java.io.IOException;
import feed.*;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.text.ParseException;
import org.xml.sax.SAXException;
import java.io.ByteArrayInputStream;

public class RssParser extends GeneralParser {
	/*
	public static void main(String[] args){
		String toParser= httpR("https://rss.nytimes.com/services/xml/rss/nyt/Technology.xml");
		XML parseo = new XML();
		try { feed.Feed f = parseo.parser(toParser);f.prettyPrint();System.out.println(f.getSiteName());}
		catch(Exception e){ System.out.println("Null f");}
	}*/
	@Override
	public feed.Feed parser(String archXML) {
		//archXML mal formado desde httpRequest
		if(archXML == null || archXML.equals("")){return null;}
		try {
			ByteArrayInputStream file = new ByteArrayInputStream( archXML.getBytes("UTF-8"));

			// Crear el parser DOM
			DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
			DocumentBuilder builder = factory.newDocumentBuilder();
			Document doc = builder.parse(file);

			// Normalizar el documento
			doc.getDocumentElement().normalize();

			//obtener el nombre del sitio                   
			NodeList seteList = doc.getElementsByTagName("channel");
			Node nodeForName = seteList.item(0);
			String name = ((Element)nodeForName).getElementsByTagName("title").item(0).getTextContent();

			feed.Feed f = new feed.Feed(name);

			// Obtener todos los elementos para el feed 
			NodeList listItems = doc.getElementsByTagName("item");

			// Recorrer los items
			for (int i = 0; i < listItems.getLength(); i++) {
				Node node = listItems.item(i);

				if (node.getNodeType() == Node.ELEMENT_NODE) {
					Element element = (Element) node;

					String title = element.getElementsByTagName(TITLE).item(0).getTextContent();
					String desc = null;
					try{
						desc= element.getElementsByTagName(DESCRIPTION).item(0).getTextContent();
					}catch(NullPointerException e){desc= "Sin descripcion"; }
					String date = element.getElementsByTagName(PUBDATE).item(0).getTextContent();
					String link = element.getElementsByTagName(LINK).item(0).getTextContent();
					Date fecha = parsingDate(date);

					feed.Article article= new feed.Article(title, desc, fecha, link);
					f.addArticle(article);
				}
			}
			return f;
		}
		catch (ParserConfigurationException | SAXException e) {
			//el xml que devuelve parser no está bien formado
			System.out.println("El archivo no está bien configurado " + e);
			return null;
		}
		catch (IOException e) {
			System.out.println("El archivo no existe o no se puede acceder" + e);
			return null;
		}
		catch (NullPointerException e) {
			//getElementsByTagName, si no está esa tag, hacer item(0) nos lleva aca.
			System.out.println("El archivo no contiene info que podemos proveer. " + e);
			return null;
		}
		catch (Exception e) {
			System.out.println("Problema desconocido. Vuelva a intentarlo. " + e);
			return null;
		}
	}
	private static  Date parsingDate(String date){
		Date fecha;
		try {
			SimpleDateFormat form = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss Z", Locale.ENGLISH);
			fecha = form.parse(date);
		}catch (ParseException e){
			//error en el parseo de la fecha por no tener esa estructura
			//se pasa una fecha por defecto, que va a ser la actual
			fecha = new Date();
		}
		return fecha;
	}

}	

