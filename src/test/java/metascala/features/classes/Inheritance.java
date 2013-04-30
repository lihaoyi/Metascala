package metascala.features.classes;

import java.util.HashMap;
import java.util.Map;
import java.util.Vector;

public class Inheritance {
    public static String implement(int n){
        Baas b = new Sheep();
        return b.baa(n);
    }
    public static String abstractClass(){
        Car toyota = new Toyota();
        return toyota.vroom();
    }

    public static String shadowedInheritedGet(){
        Car honda = new Honda();
        return honda.vroom();
    }

    public static String shadowedInheritedSet(){
        Car honda = new Honda();
        honda.rev();
        honda.cc++;
        ((Honda)honda).cc++;
        return honda.vroom();
    }

    public static String superMethod(){
        return new Toyota().superVStart();
    }
    public static int collections(int n){
        Vector<Integer> vec = new Vector<>();
        for(int i = 0; i < n; i++){
            vec.add(i);
        }
        Map<Integer, String> map = new HashMap<>();
        int total = 0;
        for(int v: vec){
            total = total + v;
            map.put(v, ""+total);
        }
        return Integer.parseInt(map.get(n/2));
    }
}
class Sheep implements Baas{
    public String baa(int n){
        String s = "b";
        for(int i = 0; i < n; i++) s = s + "a";
        return s;
    }
}
interface Baas{
    public String baa(int n);
}
class Toyota extends Car{
    public Toyota(){
        this.cc = 10;
    }

    public String vStart(){
        return "vr";
    }
    public String superVStart(){
        return super.vStart();
    }
}
class Honda extends Car{
    public int cc = 5;
    public String vStart(){
        return "v"  + cc + "r" + ((Car)this).cc + "r" + super.cc;
    }
}

class Car{
    public int cc;
    public String vStart(){
        return "";
    }
    public void rev(){
        this.cc = this.cc + 1;
    }
    public String vroom(){
        String s = vStart();
        for(int i = 0; i < cc; i++){
            s = s + "o";
        }
        return s + "m";
    }
}

