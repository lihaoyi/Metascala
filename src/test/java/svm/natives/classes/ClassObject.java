package svm.natives.classes;

public class ClassObject {
    public static String name(){
        return new ClassObject().getClass().getName();
    }
}
