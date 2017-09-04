package metascala.features.javac;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;

public class InvokeDynamic {
    final static MethodHandles.Lookup lookup = java.lang.invoke.MethodHandles.lookup();

    public static boolean findStatic0(){
        return true;
    }
    public static boolean findStaticInner() throws Throwable{
        MethodHandle m = lookup.findStatic(
                InvokeDynamic.class,
                "findStatic0",
                MethodType.methodType(boolean.class)
        );

        boolean called = (java.lang.Boolean)m.invoke();
        return called;
    }

    public static boolean findStatic(boolean b) throws Throwable{
        boolean called = findStaticInner();
        boolean result = called ^ b;
        return result;
    }



    public static boolean staticField = true;
    public static java.lang.Boolean staticFieldBoxed = true;
    public static boolean findStaticGetter(boolean b) throws Throwable{
        MethodHandle m = lookup.findStaticGetter(
            InvokeDynamic.class,
            "staticField",
            boolean.class
        );

        return (java.lang.Boolean)m.invoke() ^ b;
    }
    public static boolean findStaticGetterBoxed(boolean b) throws Throwable{
        MethodHandle m = lookup.findStaticGetter(
            InvokeDynamic.class,
            "staticFieldBoxed",
            java.lang.Boolean.class
        );

        return (java.lang.Boolean)m.invoke() ^ b;
    }

    public static boolean findStaticSetter1(boolean b) throws Throwable{
        MethodHandle m = lookup.findStaticSetter(
                InvokeDynamic.class,
                "staticField",
                boolean.class
        );
        m.invoke(false);
        return staticField ^ b;
    }
    public static boolean findStaticSetter2(boolean b) throws Throwable{
        MethodHandle m = lookup.findStaticSetter(
                InvokeDynamic.class,
                "staticFieldBoxed",
                java.lang.Boolean.class
        );
        m.invoke(false);
        return staticField ^ b;
    }

    public static boolean findStaticSetter3(boolean b) throws Throwable{
        MethodHandle m = lookup.findStaticSetter(
                InvokeDynamic.class,
                "staticField",
                boolean.class
        );
        m.invoke((java.lang.Boolean)false);
        return staticField ^ b;
    }
    public static boolean findStaticSetter4(boolean b) throws Throwable{
        MethodHandle m = lookup.findStaticSetter(
                InvokeDynamic.class,
                "staticFieldBoxed",
                java.lang.Boolean.class
        );
        m.invoke((java.lang.Boolean)false);
        return staticField ^ b;
    }



    public boolean instanceField = true;
    public static boolean findFieldGetter(boolean b) throws Throwable{
        InvokeDynamic instance = new InvokeDynamic();
        MethodHandle m = lookup.findGetter(
                InvokeDynamic.class,
                "instanceField",
                boolean.class
        );
        return (java.lang.Boolean)m.invoke(instance) ^ b;
    }

    public static boolean findFieldSetter(boolean b) throws Throwable{
        InvokeDynamic instance = new InvokeDynamic();
        MethodHandle m = lookup.findSetter(
                InvokeDynamic.class,
                "instanceField",
                boolean.class
        );
        m.invoke(instance, false);
        return instance.instanceField ^ b;
    }




    public static boolean staticMethod(boolean b){
        return !b;
    }
    public static boolean findStaticMethod(boolean b) throws Throwable{

        MethodType mt = MethodType.methodType(boolean.class, boolean.class);
        Class<?> firstParam = mt.parameterType(0);
        InvokeDynamic.class.getDeclaredMethod("staticMethod", boolean.class);
        MethodHandle m = lookup.findStatic(
                InvokeDynamic.class,
                "staticMethod",
                mt
        );
        return (java.lang.Boolean)m.invoke(b);
    }



    public static boolean run(boolean b) {
        boolean[] msg = {b};
        something(x -> msg[0] = msg[0] | x);
        return msg[0];
    }
    static void something(java.util.function.Consumer<Boolean> consumer) {
        consumer.accept(true);
    }
}
