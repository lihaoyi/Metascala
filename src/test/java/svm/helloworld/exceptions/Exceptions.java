package svm.helloworld.exceptions;

public class Exceptions {
    public static int throwCatch(){
        try{
            throw new Exception();
        }catch(Exception e){
            return 0;
        }
    }
}
