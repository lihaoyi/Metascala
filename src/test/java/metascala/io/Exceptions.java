package metascala.io;

public class Exceptions {

    public static int runtime(){
        String s = null;
        s.charAt(0);
        throw new RuntimeException("omg!");
    }

}
