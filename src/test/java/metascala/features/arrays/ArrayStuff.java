package metascala.features.arrays;

public class ArrayStuff {
    public static int[] makeIntArray(int n){
        int[] arr = new int[(short)n];
        return arr;
    }
    public static float[] makeFloatArray(){
        float[] arr = {0.25f, 0.5f, 0.75f};
        return arr;
    }
    public static long longArrayOps(int n){
        long[] arr = {12345, 67890, 12345};
        return arr[n];
    }
    public static double[] doubleArrayOps(double[] in){
        for(int i = 0; i < in.length; i++){
            in[i] = in[i] + i;
        }

        for(int i = 0; i < in.length; i++){
            in[i] *= in[i];
        }
        return in;

    }
    public static String[] makeStringArray(){
        String[] arr = {"omg", "wtf", "bbq"};
        return arr;
    }
    public static int arrayLength(){
        String[] arr0 = {};
        int[] arr1 = {1, 2, 3, 4, 5};
        double[] arr2 = {0.1, 0.2, 0.3, 0.4};
        return arr0.length + arr1.length + arr2.length;
    }
    public static int[] arraySet(){
        int[] arr = new int[10];
        for(int i = 0; i < 10; i++){
            arr[(byte)i] = i;
        }
        return arr;
    }
    public static int arrayGet(){
        int total = 0;
        int[] arr = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        for(int i: arr) total = total + i;
        for(int i = 0; i < arr.length; i++) total = total + 100 * arr[(short)i];
        return total;
    }

    public static char getSet(){
        char[] buf = new char[10];
        char[] digits = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'};
        int charPos = buf.length;
        int r = 10;
        while(charPos > 0){
            r--;
            buf[--charPos] = digits[r];
        }
        return buf[r + 5];
    }
    public static int[] bubbleSort(int[] arr){
        java.lang.reflect.Array.set(arr, 0, 100);
        return arr;
    }


}
