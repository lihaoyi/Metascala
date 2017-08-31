package metascala.features.javac;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodType;

public class InvokeDynamic {
    public static boolean handle0(){
        return true;
    }
    public static boolean handle(boolean b) throws Throwable{
        MethodHandle m = java.lang.invoke.MethodHandles.lookup().findStatic(
            InvokeDynamic.class,
            "handle0",
            MethodType.methodType(boolean.class)
        );

        return (java.lang.Boolean)m.invoke() | b;
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
