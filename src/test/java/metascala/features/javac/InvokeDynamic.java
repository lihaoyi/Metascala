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



    public long instanceField = Long.MAX_VALUE;
    public java.lang.Long instanceFieldBoxed = Long.MAX_VALUE;
    public static long findFieldGetter(long l) throws Throwable{
        InvokeDynamic instance = new InvokeDynamic();
        MethodHandle m = lookup.findGetter(
                InvokeDynamic.class,
                "instanceField",
                long.class
        );
        return (java.lang.Long)m.invoke(instance);
    }

    public static long findFieldGetterBoxed(long l) throws Throwable{
        InvokeDynamic instance = new InvokeDynamic();
        MethodHandle m = lookup.findGetter(
                InvokeDynamic.class,
                "instanceFieldBoxed",
                java.lang.Long.class
        );
        return (java.lang.Long)m.invoke(instance) - l;
    }

    public static long findFieldSetter1(long l) throws Throwable{
        InvokeDynamic instance = new InvokeDynamic();
        MethodHandle m = lookup.findSetter(
                InvokeDynamic.class,
                "instanceField",
                long.class
        );
        m.invoke(instance, l);
        return instance.instanceField;
    }

    public static long findFieldSetter2(long l) throws Throwable{
        InvokeDynamic instance = new InvokeDynamic();
        MethodHandle m = lookup.findSetter(
                InvokeDynamic.class,
                "instanceFieldBoxed",
                java.lang.Long.class
        );
        m.invoke(instance, l);
        return instance.instanceFieldBoxed;
    }

    public static long findFieldSetter3(long l) throws Throwable{
        InvokeDynamic instance = new InvokeDynamic();
        MethodHandle m = lookup.findSetter(
                InvokeDynamic.class,
                "instanceField",
                long.class
        );
        m.invoke(instance, (java.lang.Long)l);
        return instance.instanceField;
    }

    public static long findFieldSetter4(long l) throws Throwable{
        InvokeDynamic instance = new InvokeDynamic();
        MethodHandle m = lookup.findSetter(
                InvokeDynamic.class,
                "instanceFieldBoxed",
                java.lang.Long.class
        );
        m.invoke(instance, (java.lang.Long)l);
        return instance.instanceFieldBoxed;
    }





    public static boolean staticMethod(boolean b){
        return !b;
    }
    public static boolean asType(boolean b) throws Throwable{

        MethodType mt = MethodType.methodType(boolean.class, boolean.class);
        MethodHandle m = lookup.findStatic(
                InvokeDynamic.class,
                "staticMethod",
                mt
        );
        MethodHandle m2 = m.asType(
                b ? MethodType.methodType(java.lang.Boolean.class, java.lang.Boolean.class)
                  : MethodType.methodType(boolean.class, boolean.class)
        );
        return m == m2;
    }
    public static boolean findStaticMethod(boolean b) throws Throwable{

        MethodType mt = MethodType.methodType(boolean.class, boolean.class);
        MethodHandle m = lookup.findStatic(
                InvokeDynamic.class,
                "staticMethod",
                mt
        );
        if (b){
            return (boolean)m.invoke(b);
        }else{
            return (java.lang.Boolean)(m.invoke(b));
        }
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
