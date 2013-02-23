package svm.helloworld.math;

public class HelloLongs {

    public static long lmain(){ return 31337; }
    public static double dmain(){ return 31.337; }

    public static long lneg(long a){ return -a; }
    public static long ladd(long a, long b){ return a + b; }
    public static long lsub(long a, long b){ return a - b; }
    public static long lmul(long a, long b){ return a * b; }
    public static long ldiv(long a, long b){ return a / b; }
    public static long lmod(long a, long b){ return a % b; }

    public static double dneg(double a){ return -a; }
    public static double dadd(double a, double b){ return a + b; }
    public static double dsub(double a, double b){ return a - b; }
    public static double dmul(double a, double b){ return a * b; }
    public static double ddiv(double a, double b){ return a / b; }
    public static double dmod(double a, double b){ return a % b; }

    public static long lshl(long a, long b){ return a << b;}
    public static long lshr(long a, long b){ return a >> b;}
    public static long lushr(long a, long b){ return a >>> b;}
    public static long land(long a, long b){ return a & b;}
    public static long lor(long a, long b){ return a | b;}
    public static long lxor(long a, long b){ return a ^ b;}
}
