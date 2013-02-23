package svm.helloworld.controlflow;

public class Loops {
    public static int nullFor(int a){
        int c = 0;
        for(int i = 0; i > a; i++) c++;
        return c;
    }
    public static int basicFor(int a){
        int c = 1;
        for(int i = 0; i < a; i++) c = c * 2;
        return c;
    }
    public static int nullWhile(int a){
        int c = 1;
        while(c > a) c++;
        return c;
    }
    public static int basicWhile(int a){
        int c = 1;
        while(c < a) c = c * 2;
        return c;
    }
}
