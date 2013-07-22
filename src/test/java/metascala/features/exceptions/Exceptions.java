package metascala.features.exceptions;

import java.io.IOException;

public class Exceptions {
    public static int get(){
        return 0;
    }
    public static int throwCatch(){
        int a = get();
        int kk = 1;
        if (a >= 1) kk += 1;
        else        kk += 2;

        try{
            int j = a + 1;
            if(a > 0) throw new Exception();
            kk += j;
        }catch(Exception e){
            return kk - 1;
        }
        return kk;
    }

    public static int multiCatch(int in){
        try{
            try{
                try{
                    try{
                        switch (in){
                            case 0: throw new IOException("IO");
                            case 1: throw new ArrayIndexOutOfBoundsException("Array!");
                            case 2: throw new NullPointerException("NPE");
                            case 3: throw new IllegalArgumentException("IaE");
                            default: throw new Exception("Excc");
                        }
                    }catch(IOException e){
                        return 0;

                    }
                }catch(ArrayIndexOutOfBoundsException e){
                    return 1;

                }
            }catch(NullPointerException e){
                return 2;

            }
        }catch(Exception e){
            return 3;

        }
    }

    public static String nullPointer(Object o){
        try{
            return o.toString();
        }catch(NullPointerException npe){
            return "null!";
        }
    }

    public static String arrayIndexOutOfBounds(int i){
        try{
            int[] a = {1, 2, 4, 8};
            return "result! " + a[i] ;
        }catch(ArrayIndexOutOfBoundsException npe){
            return npe.getMessage();
        }
    }
}
