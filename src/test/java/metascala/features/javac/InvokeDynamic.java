package metascala.features.javac;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodType;

public class InvokeDynamic {
    public static boolean findStatic0(){
        return true;
    }
    public static boolean findStaticInner() throws Throwable{
        MethodHandle m = java.lang.invoke.MethodHandles.lookup().findStatic(
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
//        scala.Predef.println(result);
        return result;
    }

//    public static boolean findStaticGetter0 = true;
//    public static boolean findStaticGetter(boolean b) throws Throwable{
//        MethodHandle m = java.lang.invoke.MethodHandles.lookup().findStaticGetter(
//            InvokeDynamic.class,
//            "findStatic",
//            boolean.class
//        );
//
//        return (java.lang.Boolean)m.invoke() ^ b;
//    }

    public static boolean run(boolean b) {
        boolean[] msg = {b};
        something(x -> msg[0] = msg[0] | x);
        return msg[0];
    }
    static void something(java.util.function.Consumer<Boolean> consumer) {
        consumer.accept(true);
    }
}
