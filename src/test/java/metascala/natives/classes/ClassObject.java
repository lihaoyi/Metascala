package metascala.natives.classes;

public class ClassObject {
    public static String name(){
        return new ClassObject().getClass().getName();
    }
    public static String namePrim(){
        return int.class.getName();
    }
    public static String nameArray(){
        return new Object[10].getClass().getName();
    }
    public static String nameObjArray(){
        return new long[100].getClass().getName();
    }
    public static String forName(String s) throws Exception{
        Class x = Class.forName(s);
        return x.getCanonicalName();
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
