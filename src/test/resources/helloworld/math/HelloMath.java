package helloworld.math;

public class HelloMath {
    static int ia = 13;
    static int ib = 4;

    static float fa = 13;
    static float fb = 4;

    public static int imain(){ return 1337; }
    public static float fmain(){ return 1.337f; }
    public static String amain(){ return "i am 3l33t"; }

    public static int ineg(){ return -ia; }
    public static int iadd(){ return ia + ib; }
    public static int isub(){ return ia - ib; }
    public static int imul(){ return ia * ib; }
    public static int idiv(){ return ia / ib; }
    public static int imod(){ return ia % ib; }

    public static float fneg(){ return -fa; }
    public static float fadd(){ return fa + fb; }
    public static float fsub(){ return fa - fb; }
    public static float fmul(){ return fa * fb; }
    public static float fdiv(){ return fa / fb; }
    public static float fmod(){ return fa % fb; }

    public static int ishl(){ return 0x789abcde << 8;}
    public static int ishr(){ return 0x789abcde >> 8;}
    public static int iushr(){ return 0x789abcde >>> 8;}
    public static int iand(){ return 0x789abcde & 0xf0f0f0f0;}
    public static int ior(){ return 0x789abcde | 0xf0f0f0f0;}
    public static int ixor(){ return 0x789abcde ^ 0xf0f0f0f0;}
}
