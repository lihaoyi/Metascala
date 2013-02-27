package svm.helloworld.classes;

public class ClassStuff {
    public static int customClass(){
        Cow c = new Cow();
        return c.moo().length();
    }
    public static String inheritence(){
        Bull b = new Bull();
        return b.mooTwice();
    }
    public static float constructor(){
        Matrix m = new Matrix(5, 7, -1, 3);
        return m.determinant();
    }
    public static float superConstructor(){
        Matrix m = new DoubleMatrix(2, 4, -8, 4);
        return m.determinant();
    }
    public static float override(){
        Matrix m = new DoubleDetMatrix(1, 2, 3, 4);
        return m.determinant();
    }
    public static int innerClass(){
        LinkedList l = new LinkedList();
        for(int i = 0; i < 10; i++){
            l.push(i);
        }
        return l.sum();
    }
}

class Cow{
    public String moo(){
        return "moooo";
    }
}
class Bull extends Cow{
    public String mooTwice(){
        return moo() + moo();
    }
}
class Matrix{
    float aa;
    float ab;
    float ba;
    float bb;
    public Matrix(float aa, float ab, float ba, float bb){
        this.aa = aa;
        this.ab = ab;
        this.ba = ba;
        this.bb = bb;
    }
    public float determinant(){
        return aa*bb-ab*ba;
    }
}
class DoubleMatrix extends Matrix{
    public DoubleMatrix(float aa, float ab, float ba, float bb){
        super(2*aa, 2*ab, 2*ba, 2*bb);
    }
}

class DoubleDetMatrix extends Matrix{
    public DoubleDetMatrix(float aa, float ab, float ba, float bb){
        super(2*aa, 2*ab, 2*ba, 2*bb);
    }
    public float determinant(){
        return super.determinant() * 2;
    }
}
class LinkedList{
    Inner head;
    class Inner{
        int value;
        Inner next;
        public Inner(int value, Inner next){
            this.value = value;
            this.next = next;
        }
    }
    public void push(int i){
        Inner n = new Inner(i, head);
        head = n;
    }
    public int sum(){
        Inner curr = head;
        int total = 0;
        while(curr != null){
            total = total + head.value;
            curr = curr.next;
        }
        return total;
    }
}