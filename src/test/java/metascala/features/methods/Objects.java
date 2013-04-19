package metascala.features.methods;

import java.awt.*;

public class Objects {
    public static int helloWorld(int n){
        DumbObject d = new DumbObject(n);
        return d.getTwoN();
    }

    public static int inheritance(int n){
        DumbObjectSubClass d = new DumbObjectSubClass(n);
        return d.getTwoN();
    }
    public static double points(int n){
        Point p = new Point(10, 10);
        return p.getX();
        //return p.distanceSq(n, -n);
    }
    public static double points2(int n){
        Point p = new Point(10, 10);
        p.translate(5, -5); // 15 5
        p.setLocation(p.x * n, p.y * n); // 30 10
        p.setLocation(p.getY(), p.getX()); // 10 30
        p.translate(n, -n); // 15 25
        return p.distanceSq(0, n);
    }

}

class DumbObjectSubClass extends DumbObject{
    public DumbObjectSubClass(int n){
        super(n * 2);
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
