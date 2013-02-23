package svm.helloworld.math;

public class HelloMath {
    

    public static int imain(){ return 1337; }
    public static float fmain(){ return 1.337f; }
    public static String amain(){ return "i am 3l33t"; }

    public static int ineg(int a){ return -a; }
    public static int iadd(int a, int b){ return a + b; }
    public static int isub(int a, int b){ return a - b; }
    public static int imul(int a, int b){ return a * b; }
    public static int idiv(int a, int b){ return a / b; }
    public static int imod(int a, int b){ return a % b; }

    public static float fneg(float a){ return -a; }
    public static float fadd(float a, float b){ return a + b; }
    public static float fsub(float a, float b){ return a - b; }
    public static float fmul(float a, float b){ return a * b; }
    public static float fdiv(float a, float b){ return a  / b; }
    public static float fmod(float a, float b){ return a % b; }

    public static int ishl(int a, int b){ return a << b;}
    public static int ishr(int a, int b){ return a >> b;}
    public static int iushr(int a, int b){ return a >>> b;}
    public static int iand(int a, int b){ return a & b;}
    public static int ior(int a, int b){ return a | b;}
    public static int ixor(int a, int b){ return a ^ b;}
}
