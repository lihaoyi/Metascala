package metascala.features.methods;

public class Natives {
    public static float intBitsToFloat(int n){
        return Float.intBitsToFloat(n);
    }
    public static long currentTimeMillis(){
        return System.currentTimeMillis() / 100000;
    }
    public static String inheritedNative(){

        return "omg".getClass().getName();
    }
}
