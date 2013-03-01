package svm.natives.classes;

public class ClassLoaders {
    public static String name(){
        ClassLoader cl = String.class.getClassLoader();
        return "omg" + cl;
    }


}
