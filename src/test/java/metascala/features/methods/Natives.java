package metascala.features.methods;

import java.util.Arrays;

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
    public static int arrayCopy(){
        int[] x = {0, 1, 2, 3, 4};
        int[] y = {1, 2, 3, 4, 5};
        System.arraycopy(x, 1, y, 2, 2);
        return y[0] * 10000 + y[1] * 1000 + y[2] * 100 * y[3] * 10 + y[4];
    }
}
