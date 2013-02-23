package svm.helloworld.math;

public class HelloLongs {
    static long la = 13;
    static long lb = 4;
    static double da = 13;
    static double db = 4;
    public static long lmain(){ return 31337; }
    public static double dmain(){ return 31.337; }

    public static long lneg(){ return -la; }
    public static long ladd(){ return la + lb; }
    public static long lsub(){ return la - lb; }
    public static long lmul(){ return la * lb; }
    public static long ldiv(){ return la / lb; }
    public static long lmod(){ return la % lb; }

    public static double dneg(){ return -da; }
    public static double dadd(){ return da + db; }
    public static double dsub(){ return da - db; }
    public static double dmul(){ return da * db; }
    public static double ddiv(){ return da / db; }
    public static double dmod(){ return da % db; }

    public static long lshl(){ return 0x123456789abcdef0L << 16;}
    public static long lshr(){ return 0x123456789abcdef0L >> 16;}
    public static long lushr(){ return 0x123456789abcdef0L >>> 16;}
    public static long land(){ return 0x123456789abcdef0L & 0xf0f0f0f0f0f0f0f0L;}
    public static long lor(){ return 0x123456789abcdef0L | 0xf0f0f0f0f0f0f0f0L;}
    public static long lxor(){ return 0x123456789abcdef0L ^ 0xf0f0f0f0f0f0f0f0L;}
}
