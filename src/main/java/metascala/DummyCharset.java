package metascala;

import java.lang.reflect.Constructor;

/**
 * Created by lihaoyi on 12/8/17.
 */
public class DummyCharset {
    private static java.nio.charset.Charset value = null;
    static{
        try {
            Constructor<?> factory = Class.forName("sun.nio.cs.UTF_8").getConstructors()[0];
            factory.setAccessible(true);
            value = (java.nio.charset.Charset) factory.newInstance();
        }catch(Exception e){
            throw new RuntimeException(e);
        }
    }
    public static java.nio.charset.Charset getValue(){
        return value;
    }
}