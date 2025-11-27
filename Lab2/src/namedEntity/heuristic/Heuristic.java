package namedEntity.heuristic;

import java.util.Map;

public abstract class Heuristic {

	private static Map<String, String> categoryMap = Map.ofEntries(
			Map.entry("Microsft", "Company"), 
			Map.entry("Apple", "Company"), 
			Map.entry("Google", "Company"),
			Map.entry("Musk", "Person"),
			Map.entry("Biden", "Person"),
			Map.entry("Trump", "Person"),
			Map.entry("Messi", "Person"),
			Map.entry("Federer", "Person"),
			Map.entry("USA", "Country"),
			Map.entry("Russia", "Country"),
			Map.entry("Apple’s", "Company"),
			Map.entry("Trump’s", "Person"),
			Map.entry("Musk’s", "Person"),
			Map.entry("AI", "Product")
			);
	
	public String getCategory(String entity){
		return categoryMap.getOrDefault(entity, "Other");
	}
	
	
	
	public abstract boolean isEntity(String word);
		
}