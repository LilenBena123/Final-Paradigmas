package namedEntity;

import namedEntity.topic.Topic;

public class Other extends NamedEntity{
    public Other(String name, int frequency){
        super(name, "Other", frequency);
    }
    public void prettyPrint(){
	    System.out.println(
			    getName() + " " +
			    getFrequency() + " " +
                getTopic()
			    );
    }

}
