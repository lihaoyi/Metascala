package svm.helloworld.classes;

public class Interfaces {
    public static String implement(int n){
        Baas b = new Sheep();
        return b.baa(n);
    }
    public static String abstractClass(){
        Car toyota = new Toyota();
        return toyota.vroom();
    }

    public static String shadowedInherited(){
        Car honda = new Honda();
        return honda.vroom();
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
    public String vroom(){
        String s = vStart();
        for(int i = 0; i < cc; i++){
            s = s + "o";
        }
        return s + "m";
    }
}

