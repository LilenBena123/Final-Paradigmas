import feed.*;
import httpRequest.*;
import namedEntity.*;
import namedEntity.heuristic.*;
import parser.*;
import subscription.*;
import java.util.List;
import java.util.ArrayList;

public class FeedReaderMain {
	private static List<SingleSubscription> subscriptionList = new ArrayList<>();
	private static List<Feed> feeds = new ArrayList<>();

	private static void printHelp() {
		System.out.println("Please, call this program correctly: FeedReader [-ne]");
	}

	public static void main(String[] args) {
		System.out.println("************* FeedReader version 1.0 *************");
		SubscriptionParser sp = new SubscriptionParser();
		Subscription s = sp.jsonParser("../config/subscriptions.json");
		subscriptionList = s.getSubscriptionsList();
		Feed feed = null;

		for (SingleSubscription singSub : subscriptionList) {
			// Llamar al httpRequester para obtener el feed del servidor
			httpRequester http = new httpRequester();

			if (!singSub.getUrlType().equals("rss")) {
				// si entramos es porque NO es un rss
				System.out.println("Unable to parse feeds different from RSS format.");
				System.out.println("Attempted to parse feed type: " + singSub.getUrlType());
				continue;
			}

			for (int i = 0; i < singSub.getUlrParamsSize(); i++) {
				String urlFeed = singSub.getFeedToRequest(i);
				String feedContent = http.getFeedRss(urlFeed);
				RssParser rssParser = new RssParser();
				feed = rssParser.parser(feedContent);

				if (feed != null) {
					feeds.add(feed);
				}
			}
		}

		if (args.length == 0) {
			System.out.println("printing without -ne");

			for (Feed feedaux : feeds) {
				feedaux.prettyPrint();
				System.out.println("------------------------");
				System.out.println("|feed :" + feedaux.getSiteName() + "|");
				System.out.println("------------------------");
			}

		} else if (args.length == 1) {
			if (!args[0].equals("-ne")) {
				printHelp();
				return;
			}

			System.out.println("printing with -ne");
			QuickHeuristic qh = new QuickHeuristic();

			for (Feed feedaux : feeds) {
				for (Article a : feedaux.getArticleList()) {
					a.computeNamedEntities(qh);
				}
				feedaux.addAllNamedEntities();
				feedaux.prettyPrintAllNamedEntities();
				System.out.println("------------------------");
				System.out.println("|feed :" + feedaux.getSiteName() + "|");
				System.out.println("------------------------");
			}

		} else {
			printHelp();
		}
	}
}
