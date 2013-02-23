package svm.helloworld.arrays;

/**
 * Created with IntelliJ IDEA.
 * User: Haoyi
 * Date: 2/22/13
 * Time: 9:42 PM
 * To change this template use File | Settings | File Templates.
 */
public class ArrayStuff {
    public static int[] makeIntArray(){
        int[] arr = new int[5];
        return arr;
    }
    public static float[] makeFloatArray(){
        float[] arr = {0.25f, 0.5f, 0.75f};
        return arr;
    }
    public static String[] makeStringArray(){
        String[] arr = {"omg", "wtf", "bbq"};
        return arr;
    }
    public static int arrayLength(){
        int[] arr1 = {1, 2, 3, 4, 5};
        double[] arr2 = {0.1, 0.2, 0.3, 0.4};
        return arr1.length + arr2.length;
    }
    public static int[] arraySet(){
        int[] arr = new int[10];
        for(int i = 0; i < 10; i++){
            arr[i] = i;
        }
        return arr;
    }
    public static int arrayGet(){
        int total = 0;
        int[] arr = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        for(int i: arr) total = total + i;
        for(int i = 0; i < arr.length; i++) total = total + 100 * i;
        return total;
    }


}
