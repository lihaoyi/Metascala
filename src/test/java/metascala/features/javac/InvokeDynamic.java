package metascala.features.javac;

import java.util.function.IntConsumer;

public class InvokeDynamic {
    public static int run(int n) {
        int[] msg = {n};
        something(x -> msg[0] = msg[0] + x);
        return msg[0];
    }
    static void something(IntConsumer consumer) {
        consumer.accept(1337);
    }
}
