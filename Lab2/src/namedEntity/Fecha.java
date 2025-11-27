package namedEntity;

import namedEntity.topic.Topic;

public class Fecha extends NamedEntity{
    int day;
    int month;
    int year;

    public Fecha(String name, int frequency){
        super(name, "Date", frequency);
    }

    public void setDateDay(int day){
        this.day = day;
    }

    public void setDateMonth(int month){
        this.month = month;
    }

    public void setDateYear(int year){
        this.year = year;
    }

    public int getDateDay(){
        return this.day;
    }

    public int getDateMonth(){
        return this.month;
    }

    public int getDateYear(){
        return this.year;
    }

    @Override
    public String toString(){
        return "Date [date= " + this.day + ", month= " + this.month + ", year= " + this.year +"]";
    }

    @Override
    public void prettyPrint(){
	    System.out.println(
			    this.day + "/" + this.month + "/" + this.year + " " + getFrequency() + " " + getTopic()
			    );
    } 
}
