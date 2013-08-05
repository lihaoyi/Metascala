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
    public static int staticInheritance(){
        int a = Parent.x;
        Child1.x = 100;
        return a + Child1.x + Child2.x;
    }
}
interface ParentInterface{
    public static int x = 30;
}
class Parent{
    public static int x = 10;
}
class Child1 extends Parent{
    public static int get(){
        return x;
    }
}
class Cowc{}
class Child2 extends Cowc implements ParentInterface{
    public static int get(){
        return x;
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

