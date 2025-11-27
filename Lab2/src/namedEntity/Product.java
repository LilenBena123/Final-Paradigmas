package namedEntity;

import namedEntity.topic.Topic;

public class Product extends NamedEntity{
    String label;
    int cost;

    public Product(String name, int frequency){
        super(name, "Product", frequency);
    }

    public void setProductLabel(String label){
        this.label = label;
    }

    public void setProductCost(int cost){
        this.cost = cost;
    }

    public String getProductLabel(){
        return this.label;
    }

    public int getProductCost(){
        return this.cost;
    }

    @Override
    public String toString(){
        return "Event [name= "+ getName() + 
        ", label= " + label + 
        ", cost=" + cost +
        "]";
    }

    @Override
    public void prettyPrint(){
	    this.label = this.label == null ? "" : this.label;
	    System.out.println(
			    getName() + " " +
			    label + " " +
			    cost + " " +
			    getFrequency() + " " +
                this.topic.getName() 
			    );
    }

}
