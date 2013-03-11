package svm.features.math;

public class Combined {

    public static double hmsToDays(double h, double m, double s){
        return (((h*60)+m)*60+s) / 86400;
    }

}
