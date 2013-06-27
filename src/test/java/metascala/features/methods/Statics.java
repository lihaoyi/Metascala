package metascala.features.methods;

public class Statics {
    public static int helloWorld(int n){
        return timesTwo(n);
    }

    public static int timesTwo(int n){
        return n * 2;
    }

    public static int helloWorld2(int a, int b){
        return timesTwo2(a, b);
    }

    public static int timesTwo2(int a, int b){
        return (a - b) * 2;
    }

    public static int tailFactorial(int n){
        if (n == 1){
            return 1;
        }else{
            return n * tailFactorial(n-1);
        }
    }
    public static int fibonacci(int n){
        if (n == 1 || n == 0){
            return 1;
        }else{
            return fibonacci(n-1) + fibonacci(n-2);
        }
    }
    public static int call(int x) {
        return x+1;
    }
    public static int callAtPhiBoundary(int i){

        int size = (i < 0) ? 1  : call(i);
        return size;
    }
}
