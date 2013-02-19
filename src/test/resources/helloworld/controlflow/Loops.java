package helloworld.controlflow;

public class Loops {
    static int a = 10;
    final static int b = 312;

    public static int nullFor(){
        int c = 0;
        for(int i = 0; i > a; i++) c++;
        return c;
    }
    public static int basicFor(){
        int c = 1;
        for(int i = 0; i < a; i++) c = c * 2;
        return c;
    }
    public static int nullWhile(){
        int c = 1;
        while(c > 10) c++;
        return c;
    }
    public static int basicLoop(){
        int c = 1;
        while(c < 1000) c = c * 2;
        return c;
    }
}
