package metascala.features.javac;

public class InvokeDynamic {
    public static boolean run(boolean b) {
        boolean[] msg = {b};
        something(x -> msg[0] = msg[0] | x);
        return msg[0];
    }
    static void something(java.util.function.Consumer<Boolean> consumer) {
        consumer.accept(true);
    }
}
