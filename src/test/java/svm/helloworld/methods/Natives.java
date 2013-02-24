package svm.helloworld.methods;

public class Natives {
    public static float intBitsToFloat(int n){
        return Float.intBitsToFloat(n);
    }
    public static long currentTimeMillis(){
        return System.currentTimeMillis() / 100;
    }
}
