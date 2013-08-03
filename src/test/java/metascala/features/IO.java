package metascala.features;

public class IO {

    public static int retInt(){ return 1337; }
    public static double  retDouble(){ return 3.1337; }

    public static int argInt(int i){ return i; }
    public static double argDouble(double d){ return d; }

    public static double multiArgD(int i, double d){ return d; }
    public static double multiArgI(int i, double d){ return i; }

    public static String stringLiteral(){ return "omgwtfbbq"; }
    public static String strings(String s){
        if (9 * 2 + 7 > 16)
            s = "a";
        return s + "a";

    }

    public static int runtime(){
        String s = null;
        s.charAt(0);
        throw new RuntimeException("omg!");
    }
}
