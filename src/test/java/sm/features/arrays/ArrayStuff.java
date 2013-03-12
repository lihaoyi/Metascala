package sm.features.arrays;

import java.awt.geom.Point2D;

public class ArrayStuff {
    public static int[] makeIntArray(int n){
        int[] arr = new int[(short)n];
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

    public static int[] bubbleSort(int[] arr){
        for(int i = 1; i < arr.length; i++){
            for(int j = 1; j < arr.length; j++){
                if (arr[j-1] > arr[j]){
                    int temp = arr[j];
                    arr[j] = arr[j-1];
                    arr[j-1] = temp;
                }

            }
        }
        return arr;
    }
    public static void main(String[] args){

    }
}
