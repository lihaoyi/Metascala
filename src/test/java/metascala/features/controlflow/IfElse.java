package metascala.features.controlflow;

public class IfElse {
    static int a = 10;
    final static int b = 312;

    public static int basicIf(){
        if (a < b) return a;
        else return -a;
    }
    public static int ifNonIntZero(){
        if (((byte)a) > 0) return a;
        else return -a;
    }
    public static int ifNonIntBinary(){
        if (((byte)a) > (short)b) return a;
        else return -a;
    }
    public static int ifElseIf(){
        if (a > b) return a;
        else if (a == b) return -a;
        else return b;
    }
    public static int ifElseIfBig(){
        if (a > b) return 1;
        else if (a > 12) return 2;
        else if (b < 10) return 3;
        else if (b == a) return 4;
        else if (b > a) return 5;
        else if (a == 10) return 6;
        else if (b == 312) return 7;
        else return 8;
    }
    public static int mathMin(){
        return Math.min(16, 1);
    }
}
