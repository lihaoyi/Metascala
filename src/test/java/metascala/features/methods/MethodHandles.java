package metascala.features.methods;

import java.lang.invoke.MethodHandle;

import java.lang.invoke.MethodType;

public class MethodHandles {
    final static java.lang.invoke.MethodHandles.Lookup lookup = java.lang.invoke.MethodHandles.lookup();

    public static boolean findStatic0(){
        return true;
    }
    public static boolean findStaticInner() throws Throwable{
        MethodHandle m = lookup.findStatic(
                MethodHandles.class,
                "findStatic0",
                MethodType.methodType(boolean.class)
        );

        boolean called = (Boolean)m.invoke();
        return called;
    }

    public static boolean findStatic(boolean b) throws Throwable{
        boolean called = findStaticInner();
        boolean result = called ^ b;
        return result;
    }



    public static boolean staticField = true;
    public static Boolean staticFieldBoxed = true;
    public static boolean findStaticGetter(boolean b) throws Throwable{
        MethodHandle m = lookup.findStaticGetter(
            MethodHandles.class,
            "staticField",
            boolean.class
        );

        return (Boolean)m.invoke() ^ b;
    }
    public static boolean findStaticGetterBoxed(boolean b) throws Throwable{
        MethodHandle m = lookup.findStaticGetter(
            MethodHandles.class,
            "staticFieldBoxed",
            Boolean.class
        );

        return (Boolean)m.invoke() ^ b;
    }

    public static boolean findStaticSetter1(boolean b) throws Throwable{
        MethodHandle m = lookup.findStaticSetter(
                MethodHandles.class,
                "staticField",
                boolean.class
        );
        m.invoke(false);
        return staticField ^ b;
    }
    public static boolean findStaticSetter2(boolean b) throws Throwable{
        MethodHandle m = lookup.findStaticSetter(
                MethodHandles.class,
                "staticFieldBoxed",
                Boolean.class
        );
        m.invoke(false);
        return staticField ^ b;
    }

    public static boolean findStaticSetter3(boolean b) throws Throwable{
        MethodHandle m = lookup.findStaticSetter(
                MethodHandles.class,
                "staticField",
                boolean.class
        );
        m.invoke((Boolean)false);
        return staticField ^ b;
    }
    public static boolean findStaticSetter4(boolean b) throws Throwable{
        MethodHandle m = lookup.findStaticSetter(
                MethodHandles.class,
                "staticFieldBoxed",
                Boolean.class
        );
        m.invoke((Boolean)false);
        return staticField ^ b;
    }



    public long instanceField = Long.MAX_VALUE;
    public Long instanceFieldBoxed = Long.MAX_VALUE;
    public static long findFieldGetter(long l) throws Throwable{
        MethodHandles instance = new MethodHandles();
        MethodHandle m = lookup.findGetter(
                MethodHandles.class,
                "instanceField",
                long.class
        );
        return (Long)m.invoke(instance);
    }

    public static long findFieldGetterBoxed(long l) throws Throwable{
        MethodHandles instance = new MethodHandles();
        MethodHandle m = lookup.findGetter(
                MethodHandles.class,
                "instanceFieldBoxed",
                Long.class
        );
        return (Long)m.invoke(instance) - l;
    }

    public static long findFieldSetter1(long l) throws Throwable{
        MethodHandles instance = new MethodHandles();
        MethodHandle m = lookup.findSetter(
                MethodHandles.class,
                "instanceField",
                long.class
        );
        m.invoke(instance, l);
        return instance.instanceField;
    }

    public static long findFieldSetter2(long l) throws Throwable{
        MethodHandles instance = new MethodHandles();
        MethodHandle m = lookup.findSetter(
                MethodHandles.class,
                "instanceFieldBoxed",
                Long.class
        );
        m.invoke(instance, l);
        return instance.instanceFieldBoxed;
    }

    public static long findFieldSetter3(long l) throws Throwable{
        MethodHandles instance = new MethodHandles();
        MethodHandle m = lookup.findSetter(
                MethodHandles.class,
                "instanceField",
                long.class
        );
        m.invoke(instance, (Long)l);
        return instance.instanceField;
    }

    public static long findFieldSetter4(long l) throws Throwable{
        MethodHandles instance = new MethodHandles();
        MethodHandle m = lookup.findSetter(
                MethodHandles.class,
                "instanceFieldBoxed",
                Long.class
        );
        m.invoke(instance, (Long)l);
        return instance.instanceFieldBoxed;
    }





    public static double staticMethod(double b){
        return b * b;
    }
    public static boolean asType(boolean b) throws Throwable{

        MethodType mt = MethodType.methodType(double.class, double.class);
        MethodHandle m = lookup.findStatic(
                MethodHandles.class,
                "staticMethod",
                mt
        );
        MethodHandle m2 = m.asType(
                b ? MethodType.methodType(Double.class, Double.class)
                  : MethodType.methodType(double.class, double.class)
        );
        return m == m2;
    }
    public static double findStaticMethod(double d) throws Throwable{

        MethodType mt = MethodType.methodType(double.class, double.class);
        MethodHandle m = lookup.findStatic(
                MethodHandles.class,
                "staticMethod",
                mt
        );
        if (d < 0){
            return (double)m.invoke(d);
        }else{
            return (Double)(m.invoke((Double)d));
        }
    }




    public char virtualMethod(char input, int offset) {
        if (Character.isLowerCase(input + offset)) {
            return Character.toUpperCase((char)(input + offset));
        }
        else {
            return Character.toLowerCase((char)(input + offset));
        }
    }

    public static char findVirtualMethod(char c) throws Throwable{

        MethodType mt = MethodType.methodType(char.class, char.class, int.class);
        MethodHandle m = lookup.findVirtual(
                MethodHandles.class,
                "virtualMethod",
                mt
        );
        if (Character.isUpperCase(c)){
            return (char)m.invoke(new MethodHandles(), c, 3);
        }else{
            return (Character)(m.invoke(
                    new MethodHandles(),
                    (Character)c, 4
            ));
        }
    }





    private char specialMethod(char input, int offset) {
        if (Character.isLowerCase(input + offset)) {
            return Character.toUpperCase((char)(input + offset));
        }
        else {
            return Character.toLowerCase((char)(input + offset));
        }
    }

    public static char findSpecialMethod(char c) throws Throwable{

        MethodType mt = MethodType.methodType(char.class, char.class, int.class);
        MethodHandle m = lookup.findSpecial(
                MethodHandles.class,
                "specialMethod",
                mt,
                MethodHandles.class
        );
        if (Character.isUpperCase(c)){
            return (char)m.invoke(new MethodHandles(), c, 3);
        }else{
            return (Character)(m.invoke(
                    new MethodHandles(),
                    (Character)c, 4
            ));
        }
    }





    public static char findInterfaceMethod(int index) throws Throwable{
        MethodType mt = MethodType.methodType(char.class, int.class);
        MethodHandle m = lookup.findVirtual(
                CharSequence.class,
                "charAt",
                mt
        );

        if (index % 2 == 0) {

            return (char)m.invoke("Hello", index);
        }else{
            return (Character)m.invoke("World", (Integer)index);
        }
    }




    public static double negate(double d){
        return -d;
    }

    public static double triple(double d){
        return  3 * d;
    }

    public static double transformStaticMethod(double d) throws Throwable{

        MethodType mt = MethodType.methodType(double.class, double.class);
        MethodHandle m = lookup.findStatic(
                MethodHandles.class,
                "staticMethod",
                mt
        );

        MethodHandle m2 = java.lang.invoke.MethodHandles.filterArguments(
                m,
                0,
                lookup.findStatic(MethodHandles.class, "triple", mt)
        );
        MethodHandle m3 = java.lang.invoke.MethodHandles.insertArguments(m2, 0, d);
        MethodHandle m4 = java.lang.invoke.MethodHandles.filterReturnValue(
                m3,
                lookup.findStatic(MethodHandles.class, "negate", mt)
        );

        return (double)m4.invoke();
    }


    public static int count(Object... input){
        return input.length;
    }

    public static boolean varargs(boolean b) throws Exception{

        lookup.findStatic(
                MethodHandles.class,
                "count",
                MethodType.methodType(int.class, Object[].class)
        );

//        boolean[] msg = {b};
//        something(x -> msg[0] = msg[0] | x);
        return true;
    }

    public static boolean lambda(boolean b) throws Exception{
        boolean[] msg = {b};
        java.util.function.Consumer<Boolean> consumer = x -> msg[0] = msg[0] | x;
        consumer.accept(true);
        return msg[0];
    }
}
