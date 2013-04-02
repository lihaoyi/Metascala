package metascala.features;

import java.awt.geom.Point2D;

public class Casting {
    public static int basicCast(Object x){
        try{
            String s = (String)x;
            return s.length();
        }catch(ClassCastException e){
            return -1;
        }
    }
    public static int arrayCasts(Object x){
        try{
            Point2D[] s = (Point2D[])x;
            return s.length;
        }catch(ClassCastException e){
            return -1;
        }
    }

    public static int primArrayCasts(Object x){
        try{
            int[] s = (int[])x;
            return s.length;
        }catch(ClassCastException e){
            return -1;
        }
    }
    public static int instanceOf(Object x){
        return x instanceof Point2D ? 0 : 1;
    }
    public static int instanceOfArray(Object x){
        return x instanceof Point2D[] ? 0 : 1;
    }
    public static int instanceOfPrimArray(Object x){
        return x instanceof int[] ? 0 : 1;
    }
}
