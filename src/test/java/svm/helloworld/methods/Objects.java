package svm.helloworld.methods;

public class Objects {
    public static int helloWorld(int n){
        DumbObject d = new DumbObject(n);
        return d.getTwoN();
    }
}
class DumbObject{
    int n = 0;
    public DumbObject(int n){
        this.n = n;
    }
    public int getTwoN(){
        return n * 2;
    }
}
