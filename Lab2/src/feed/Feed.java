package feed;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import namedEntity.*;
import namedEntity.heuristic.*;
import namedEntity.topic.*;
import namedEntity.topic.Tecnologia;

/*Esta clase modela la lista de articulos de un determinado feed*/
public class Feed {
	String siteName;
	List<Article> articleList;
	List<Person> personList = new ArrayList<Person>();
	List<Fecha> dateList = new ArrayList<Fecha>();
	List<Event> eventList = new ArrayList<Event>();
	List<Lugar> placeList = new ArrayList<Lugar>();
	List<Organization> organizationList = new ArrayList<Organization>();
	List<Product> productList = new ArrayList<Product>();
	List<Other> otherList = new ArrayList<Other>();

	public Feed(String siteName) {
		super();
		this.siteName = siteName;
		this.articleList = new ArrayList<Article>();
	}

	public String getSiteName() {
		return siteName;
	}

	public void setSiteName(String siteName) {
		this.siteName = siteName;
	}

	public List<Article> getArticleList() {
		return articleList;
	}

	// Estas son las listas de las NE
	public List<Person> getPersonList() {
		return this.personList;
	}

	public List<Event> getEventList() {
		return this.eventList;
	}

	public List<Fecha> getDateList() {
		return this.dateList;
	}

	public List<Product> getProductList() {
		return this.productList;
	}

	public List<Organization> getOrganizationList() {
		return this.organizationList;
	}

	public List<Lugar> getPlaceList() {
		return this.placeList;
	}

	public List<Other> getOtherList() {
		return this.otherList;
	}

	// fin de getter de las listas de NE
	public void setArticleList(List<Article> articleList) {
		this.articleList = articleList;
	}

	public void addArticle(Article a) {
		this.getArticleList().add(a);
	}

	// añadir objeto NE a una lista global:

	public List<NamedEntity> getAllNamedEntities() {
		Map<String, NamedEntity> globalMap = new HashMap<>();

		for (Article a : this.getArticleList()) {
			for (NamedEntity ne : a.getList()) {
				String key = ne.getName() + "::" + ne.getCategory(); // clave única

				if (globalMap.containsKey(key)) {
					globalMap.get(key).incFrequency();
				} else {
					globalMap.put(key, new NamedEntity(ne.getName(), ne.getCategory(), ne.getFrequency()));
				}
			}
		}

		return new ArrayList<>(globalMap.values());
	}

	public void createTopic(NamedEntity entity){
		String topic = entity.getTopicc(entity.getName());
		if(topic.equals("Futbol")){
					Futbol fut = new Futbol(topic);
					entity.setTopic(fut);
				}else if(topic.equals("Tenis")){
					Tenis ten = new Tenis(topic);
					entity.setTopic(ten);
				}else if(topic.equals("International")){
					Internacional politics = new Internacional(topic);
					entity.setTopic(politics);
				}else if(topic.equals("Tecnology")){
					Tecnologia tecno = new Tecnologia(topic);
					entity.setTopic(tecno);
				}else{
					Otro other = new Otro(topic);
					entity.setTopic(other);
				}
	}

	public void addAllNamedEntities() {
		for (NamedEntity n : this.getAllNamedEntities()) {
			String category = n.getCategory();
			if (category.equals("Person")) {
				//Create the Person instance
				Person p = new Person(n.getName(), n.getFrequency());
				createTopic(p);
				this.personList.add(p);
			} else if (category.equals("Fecha")) {
				Fecha date = new Fecha(n.getName(), n.getFrequency());
				createTopic(date);
				this.dateList.add(date);
			} else if (category.equals("Other")) {
				Other other = new Other(n.getName(), n.getFrequency());
				createTopic(other);
				this.otherList.add(other);
			} else if (category.equals("Company")) {
				Organization o = new Organization(n.getName(), n.getFrequency());
				createTopic(o);
				this.organizationList.add(o);
			} else if (category.equals("Event")) {
				Event event = new Event(n.getName(), n.getFrequency());
				createTopic(event);
				this.eventList.add(event);
			} else if (category.equals("Country")) {
				Lugar lugar = new Lugar(n.getName(), n.getFrequency());
				createTopic(lugar);
				this.placeList.add(lugar);
			} else if (category.equals("Product")) {
				Product pro = new Product(n.getName(), n.getFrequency());
				createTopic(pro);
				this.productList.add(pro);
			}
		}
	}

	public Article getArticle(int i) {
		return this.getArticleList().get(i);
	}

	public int getNumberOfArticles() {
		return this.getArticleList().size();
	}

	// fun getNumber de las NE
	public int getNumberOfPersons() {
		return this.getPersonList().size();
	}

	public int getNumberOfOthers() {
		return this.getOtherList().size();
	}

	public int getNumberOfPlaces() {
		return this.getPlaceList().size();
	}

	public int getNumberOfOrganizations() {
		return this.getOrganizationList().size();
	}

	public int getNumberOfDates() {
		return this.getDateList().size();
	}

	public int getNumberOfProducts() {
		return this.getProductList().size();
	}

	public int getNumberOfEvents() {
		return this.getEventList().size();
	}

	// fin de getNumber NE
	@Override
	public String toString() {
		return "Feed [siteName=" + getSiteName() + ", articleList=" + getArticleList() + "]";
	}

	public void prettyPrint() {
		for (Article a : this.getArticleList()) {
			a.prettyPrint();
		}

	}

	public void prettyPrintPerson() {
		System.out.println("[PERSON]");
		for (Person p : getPersonList()) {
			System.out.println("  - " + p.getName() + " (" + p.getFrequency() + ")" + " " + p.getTopic());
		}
	}

	public void prettyPrintProduct() {
		System.out.println("[PRODUCT]");
		for (Product p : getProductList()) {
			System.out.println("  - " + p.getName() + " (" + p.getFrequency() + ")" + " " + p.getTopic());
		}
	}

	public void prettyPrintEvent() {
		System.out.println("[EVENT]");
		for (Event e : getEventList()) {
			System.out.println("  - " + e.getName() + " (" + e.getFrequency() + ")" + " " + e.getTopic());
		}
	}

	public void prettyPrintPlace() {
		System.out.println("[PLACE] ");
		for (Lugar l : getPlaceList()) {
			System.out.println("  - " + l.getName() + " (" + l.getFrequency() + ")" + " " + l.getTopic());
		}
	}

	public void prettyPrintDate() {
		System.out.println("[DATE]");
		for (Fecha f : getDateList()) {
			System.out.println("  - " + f.getName() + " (" + f.getFrequency() + ")" + " " + f.getTopic());
		}
	}

	public void prettyPrintOrganization() {
		System.out.println("[ORGANIZATION]");
		for (Organization o : getOrganizationList()) {
			System.out.println("  - " + o.getName() + " (" + o.getFrequency() + ")" + " " + o.getTopic());
		}
	}

	public void prettyPrintOther() {
		System.out.println("[OTHER]");
		for (Other x : getOtherList()) {
			System.out.println("  - " + x.getName() + " (" + x.getFrequency() + ")" + " " + x.getTopic());
		}
	}

	public void prettyPrintAllNamedEntities() {
		System.out.println("");
		System.out.println("******************* Named Entities *******************");

		prettyPrintPerson();
		prettyPrintProduct();
		prettyPrintEvent();
		prettyPrintPlace();
		prettyPrintDate();
		prettyPrintOrganization();
		prettyPrintOther();
	}

	public static void main(String[] args) {
		Article a1 = new Article("This Historically Black University Created Its Own Tech Intern Pipeline",
				"A new program at Bowie State connects computing students directly with companies, bypassing an often harsh Silicon Valley vetting process",
				new Date(),
				"https://www.nytimes.com/2023/04/05/technology/bowie-hbcu-tech-intern-pipeline.html");

		Article a2 = new Article("This Historically Black University Created Its Own Tech Intern Pipeline",
				"A new program at Bowie State connects computing students directly with companies, bypassing an often harsh Silicon Valley vetting process",
				new Date(),
				"https://www.nytimes.com/2023/04/05/technology/bowie-hbcu-tech-intern-pipeline.html");

		Article a3 = new Article("This Historically Black University Created Its Own Tech Intern Pipeline",
				"A new program at Bowie State connects computing students directly with companies, bypassing an often harsh Silicon Valley vetting process",
				new Date(),
				"https://www.nytimes.com/2023/04/05/technology/bowie-hbcu-tech-intern-pipeline.html");

		Article a4 = new Article("Trump and Biden and Musk and Elon Musk were seen in Russia with love",
				"A new program at Bowie State connects computing students directly with companies, bypassing an often harsh Silicon Valley vetting process",
				new Date(),
				"https://www.nytimes.com/2023/04/05/technology/bowie-hbcu-tech-intern-pipeline.html");
		Feed f = new Feed("nytimes");
		f.addArticle(a1);
		f.addArticle(a2);
		f.addArticle(a3);
		f.addArticle(a4);

		QuickHeuristic qh = new QuickHeuristic();

		for (Article a : f.getArticleList()) {
			a.computeNamedEntities(qh);
		}
		
		f.addAllNamedEntities();

		System.out.println("num personas " + f.getNumberOfPersons());

		System.out.println("num fechas" + f.getNumberOfDates());
		System.out.println("num Lugares " + f.getNumberOfPlaces());
		System.out.println("num Organiz. " + f.getNumberOfOrganizations());
		System.out.println("num produ " + f.getNumberOfProducts());
		System.out.println("num eventos " + f.getNumberOfEvents());
		System.out.println("num otros " + f.getNumberOfOthers());

		/*
		 * prettyPrintPerson();
		 * prettyPrintProduct();
		 * prettyPrintEvent();
		 * prettyPrintPlace();
		 * prettyPrintDate();
		 * prettyPrintOrganization();
		 * prettyPrintOther();
		 */
		f.prettyPrintAllNamedEntities();
	}

}