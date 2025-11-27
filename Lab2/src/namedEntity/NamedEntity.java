package namedEntity;

import java.util.Map;
import namedEntity.topic.*;

/*Esta clase modela la nocion de entidad nombrada*/

public abstract class NamedEntity {

    String name;
    String category;
    int frequency;
    Topic topic;
    private static Map<String, String> topicMap = Map.ofEntries(
            Map.entry("Microsoft", "Tecnology"),
            Map.entry("Apple", "Tecnology"),
            Map.entry("Google", "Tecnology"),
            Map.entry("Musk", "Tecnology"),
            Map.entry("Biden", "International"),
            Map.entry("Trump", "International"),
            Map.entry("Messi", "Futbol"),
            Map.entry("Federer", "Tenis"),
            Map.entry("USA", "Politics"),
            Map.entry("Russia", "Politics"),
            Map.entry("Apple's", "Tecnology"),
            Map.entry("Trump's", "International"),
            Map.entry("Musk's", "Tecnology"),
            Map.entry("AI", "Tecnology")
    );

    public abstract String getEntityType();

    public NamedEntity(String name, String category, int frequency) {
        super();
        this.name = name;
        this.category = category;
        this.frequency = frequency;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setTopic(Topic topic) {
        this.topic = topic;
    }

    public String getCategory() {
        return this.category;
    }

    public String getTopic() {
        return this.topic.getName();
    }

    public void setCategory(String category) {
        this.category = category;
    }

    public int getFrequency() {
        return frequency;
    }

    public String getTopicc(String entity) {
        return topicMap.getOrDefault(entity, "Other");
    }

    public void setFrequency(int frequency) {
        this.frequency = frequency;
    }

    public void incFrequency() {
        this.frequency++;
    }

    @Override
    public String toString() {
        return "ObjectNamedEntity [name=" + name + ", frequency=" + frequency + "]";
    }

    public void prettyPrint() {
        System.out.println(this.getName() + " " + this.getFrequency());
    }

}
