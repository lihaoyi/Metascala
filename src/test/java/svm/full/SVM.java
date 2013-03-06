package svm.full;

import svm.Util$;
import org.mozilla.javascript.*;
public class SVM {
    public void initSVM(){
        Util$.MODULE$.singleClassVm("svm.math.HelloMath");
    }
    public static String rhino(){
        Context cx = Context.enter();
        Scriptable scope = cx.initStandardObjects();
        String s = "10";
        Object result = cx.evaluateString(scope, s, "<cmd>", 1, null);
        return ""+result;
    }
    public static void main(String[] args){
        System.out.println(int.class);
        System.out.println(Integer.class);
        System.out.println(new int[0].getClass() == new int[10].getClass());
        System.out.println(new Integer[0].getClass());
    }

}
