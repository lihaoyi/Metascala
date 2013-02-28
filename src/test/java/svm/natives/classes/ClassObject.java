package svm.natives.classes;

public class ClassObject {
    public static String name(){
        return new ClassObject().getClass().getName()
                + new long[100].getClass().getName();
    }
    public static String forName(String s){
        try{
            return Class.forName(s).getName();
        }catch(ClassNotFoundException c){
            return c.getMessage();
        }
    }
    public static boolean[] isPrimitive(){
        return new boolean[]{
                new ClassObject().getClass().isPrimitive(),
                new Float(10).getClass().isPrimitive(),
                new Integer(12).getClass().isPrimitive(),
                new Boolean(true).getClass().isPrimitive(),
                new int[0].getClass().isPrimitive()
        };
    }

    public static boolean[] isArray(){
        return new boolean[]{
                new ClassObject().getClass().isArray(),
                new Float(10).getClass().isArray(),
                new Integer(12).getClass().isArray(),
                new Boolean(true).getClass().isArray(),
                new int[0].getClass().isArray()
        };
    }

}
