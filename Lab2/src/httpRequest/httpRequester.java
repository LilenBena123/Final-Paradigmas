package httpRequest;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

/* Esta clase se encarga de realizar efectivamente el pedido de feed al servidor de noticias
 * Leer sobre como hacer una http request en java
 * https://www.baeldung.com/java-http-request
 * */

public class httpRequester {

	private final HttpClient client;

	public httpRequester() {
		this.client = HttpClient.newHttpClient();
	}

	public String getFeedRss(String urlFeed) {
		String feedRssXml = fetchFeed(urlFeed);
		return feedRssXml;
	}

	public String getFeedReedit(String urlFeed) {
		String feedReeditJson = null;
		return feedReeditJson;
	}

	private String fetchFeed(String urlFeed) {
		try {
			HttpRequest request = HttpRequest.newBuilder()
					.uri(URI.create(urlFeed))
					.GET()
					.build();

			HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

			if (response.statusCode() == 200) {
				return response.body();
			} else {
				System.out.println("Error: status code " + response.statusCode());
			}
		} catch (IOException | InterruptedException e) {
			System.out.println("Error fetching feed from " + urlFeed + ": " + e.getMessage());
		}

		return null;
	}

	// Clase de prueba
	public static void main(String[] args) {
		// URL del feed de noticias
		String urlFeed = "https://rss.nytimes.com/services/xml/rss/nyt/Business.xml";

		// Crear instancia de httpRequester
		httpRequester requester = new httpRequester();

		// Obtener el feed
		String feedContent = requester.getFeedRss(urlFeed);

		// Imprimir el resultado
		if (feedContent != null) {
			System.out.println("Contenido del feed RSS:");
			System.out.println(feedContent);
		} else {
			System.out.println("No se pudo obtener el feed.");
		}
	}
}
