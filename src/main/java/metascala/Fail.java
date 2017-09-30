package metascala;

/**
 * Stubs used for testing to make sure our error handling works properly in the
 * case of internal VM failures
 */
public class Fail {
    /**
     * Call this to make metascala fail with an `InternalVmException`, since
     * this method is defined with no implementation neither in Java nor in
     * the default bindings
     */
    public native static void missingNativeMethodImplementation();

    /**
     * Call this to make metascala fail with nested `InternalVmException`s:
     * with the call stack comprising mixed JVM -> internal -> JVM -> internal
     * sections at time of failure
     */
    public native static void indirectMissingNativeMethodImplementation();

    /**
     * Called by the native implementation of `indirectMissingNativeMethodImplementation`
     */
    public static int indirectMissingNativeMethodImplementation0(int input){
        int i = input + 1;
        missingNativeMethodImplementation();
        return i;
    }
}