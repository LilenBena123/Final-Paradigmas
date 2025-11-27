package namedEntity;

import namedEntity.topic.Topic;

public class Event extends NamedEntity{
    String date;
    String location;

    public Event(String name, int frequency){
        super(name, "Event", frequency);
    }

    public void setEventDate(String date){
        this.date = date;
    }

    public void setEventLocation(String location){
        this.location = location;
    }

    public String getEventDate(){
        return this.date;
    }

    public String getEventLocation(){
        return this.location;
    }

    @Override
    public String toString(){
        return "Event [name=" + getName() + 
        ", date=" + date +
        ", location=" + location +
        "freq=" + getFrequency() + "]";
    }

    @Override
    public void prettyPrint(){
	    this.date = this.date == null ? "" : this.date;
	    this.location = this.location == null ? "" : this.location;

	    System.out.println(
			    getName() + " " +
			    date + " " +
			    location + " " +
			    getFrequency() + " " +
                getTopic()
			    );
    }
}
