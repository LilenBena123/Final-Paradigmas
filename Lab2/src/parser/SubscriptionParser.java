package parser;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.charset.StandardCharsets;
import subscription.Subscription;
import subscription.SingleSubscription;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;
import java.util.ArrayList;            
import org.json.JSONArray;
import org.json.JSONObject;
import org.json.JSONTokener;
import feed.Feed;

/*
 * Esta clase implementa el parser del  archivo de suscripcion (json)
 * Leer https://www.w3docs.com/snippets/java/how-to-parse-json-in-java.html
 * */

public class SubscriptionParser extends GeneralParser{
    public Subscription jsonParser(String path){
        // First, check if the path exists
        if (fileExists(path)){
            // Logic for the parsing the JSON
            try{
                Subscription result = new Subscription(path);
                // Create a string with the info of the path
                String content = new String(Files.readAllBytes(Paths.get(path)), StandardCharsets.UTF_8);
                // Create a new array for the JSON objects
                JSONArray list_obj = new JSONArray(content);
        

                // Iterate in the list of the JSON
                for(int i = 0; i < list_obj.length(); i++){
                    JSONObject elem = list_obj.getJSONObject(i);

                    String url = elem.getString("url");
                    String urlType = elem.getString("urlType");
                    String download = elem.getString("download");

                    //Now parse the list of urlParams
                    JSONArray list = elem.getJSONArray("urlParams");
                    SingleSubscription sub = new SingleSubscription(url, null, urlType);
                    for(int j = 0; j < list.length(); j++){
                        String p = list.getString(j);
                        sub.setUlrParams(p);
                    }
                    // Parse the line in  SingleSuscription object
                    result.addSingleSubscription(sub);
                }
                return result;
            }
            catch (IOException e){
                System.out.println("Error in create a fileReader\n");
                return null;
            } 
        }else{
            System.out.println("The file doesn´t exist!!!.\n");
            return null;
        }
    }

    @Override 
    public feed.Feed parser(String arch){
        System.out.println("I can´t read this format of JSON, sorry!!!\n");
        return null;
    }
}
