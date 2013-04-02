package metascala.natives.classes;

public class ClassLoaders {
    public static String name(){
        ClassLoader cl = String.class.getClassLoader();
        return "omg" + cl;
    }
    public static String create(){
        ClassLoader cl = new ClassLoader(){};

        return "omg" + cl;
    }
    public static String load(final byte[] classData) throws Exception{
        ClassLoader cl = new ClassLoader(){
            protected Class<?> findClass(String name) throws ClassNotFoundException {
                return defineClass(name, classData, 0, classData.length);
            }
        };
        Class c = cl.loadClass("omg");
        return "omg" + cl;
    }


}
