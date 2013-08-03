package metascala.full;

import java.util.*;
import java.math.BigInteger;
import java.util.concurrent.atomic.*;
import java.util.regex.*;
public class JavaLib {

    public static int collections(int n){
        Vector<Integer> vec = new Vector<>();
        for(int i = 0; i < n; i++){
            vec.add(i);
        }
        Map<Integer, String> map = new HashMap<>();
        int total = 0;
        for(int v: vec){
            total = total + v;
            map.put(v, ""+total);
        }

        return Integer.parseInt(map.get(n/2));
    }

    public static int sorting(){
        int[] arr = new int[250];
        int current = 94664704;
        for(int i = 1; i < arr.length; i++){
            current = 23 * current % 100000000 + 1;
            arr[i] = current % 100000;
        }

        Arrays.sort(arr);

        return arr[52];
    }

    public static String sudoku(){
        int[][] grid = {
            {5, 3, 0, 0, 7, 0, 0, 0, 0},
            {6, 0, 0, 1, 9, 5, 0, 0, 0},
            {0, 9, 8, 0, 0, 0, 0, 6, 0},
            {8, 0, 0, 0, 6, 0, 0, 0, 3},
            {4, 0, 0, 8, 0, 3, 0, 0, 1},
            {7, 0, 0, 0, 2, 0, 0, 0, 6},
            {0, 6, 0, 0, 0, 0, 2, 8, 0},
            {0, 0, 0, 4, 1, 9, 0, 0, 5},
            {0, 0, 0, 0, 8, 0, 0, 7, 9},
        };
        Sudoku.solve(0, 0, grid);
        return Sudoku.writeMatrix(grid);
    }


    public static String bigInteger(){
        BigInteger a = new BigInteger("1237923896771092385");
        BigInteger b = new BigInteger("498658982734992345912340");
        BigInteger c = new BigInteger("08968240235478367717203984123");

        BigInteger d = a.add(b);
        BigInteger e = d.subtract(c);
        BigInteger f = e.multiply(b);
        BigInteger g = f.divide(a);
        return g.toString();
    }

    public static String regex(){
        Pattern p = Pattern.compile("\\d+([_-]\\d+)*(:? )");
        Matcher m = p.matcher("123_321_12 i am a cow 123_3-" +
                "12_990 but my ip is 192-168-1-1 lolz");

        String s = "";
        while(m.find()){
            s += m.group(0);
        }

        return s;
    }
    public static void main(String[] args){

    }
    public static boolean[] atomicBooleans(){
        AtomicBoolean b = new AtomicBoolean();
        boolean[] values = new boolean[4];
        b.set(true);
        values[0] = b.get();
        b.compareAndSet(false, false);
        values[1] = b.get();
        values[2] = b.getAndSet(false);
        b.compareAndSet(false, true);
        values[3] = b.get();
        return values;
    }
    public static int[] atomicIntegers(){
        AtomicInteger b = new AtomicInteger();
        int[] values = new int[4];
        b.set(192);
        values[0] = b.get();
        b.compareAndSet(12, 3123);
        values[1] = b.get();
        values[2] = b.getAndSet(31241);
        b.compareAndSet(31241, 12451);
        values[3] = b.get();
        return values;
    }
    public static long[] atomicLongs(){
        AtomicLong b = new AtomicLong();
        long[] values = new long[4];
        b.set(1921231231234124124L);
        values[0] = b.get();
        b.compareAndSet(12124124164865234L, 34934198359342123L);
        values[1] = b.get();
        values[2] = b.getAndSet(98172271923198732L);
        b.compareAndSet(981724127399231987L, 123517894187923123L);
        values[3] = b.get();
        return values;
    }
    public static long randoms(){
        Random r = new Random(241231241251241123L);
        for(int i = 0; i < 100; i++){
            r.nextLong();
        }
        return r.nextLong();
    }

}
class Sudoku {
    static boolean solve(int i, int j, int[][] cells) {
        if (i == 9) {
            i = 0;
            if (++j == 9)
                return true;
        }
        if (cells[i][j] != 0)  // skip filled cells
            return solve(i+1,j,cells);

        for (int val = 1; val <= 9; ++val) {
            if (legal(i,j,val,cells)) {
                cells[i][j] = val;
                if (solve(i+1,j,cells))
                    return true;
            }
        }
        cells[i][j] = 0; // reset on backtrack
        return false;
    }

    static boolean legal(int i, int j, int val, int[][] cells) {
        for (int k = 0; k < 9; ++k)  // row
            if (val == cells[k][j])
                return false;

        for (int k = 0; k < 9; ++k) // col
            if (val == cells[i][k])
                return false;

        int boxRowOffset = (i / 3)*3;
        int boxColOffset = (j / 3)*3;
        for (int k = 0; k < 3; ++k) // box
            for (int m = 0; m < 3; ++m)
                if (val == cells[boxRowOffset+k][boxColOffset+m])
                    return false;

        return true; // no violations, so it's legal
    }

    static String writeMatrix(int[][] solution) {

        StringBuilder s = new StringBuilder("\n");
        for (int i = 0; i < 9; ++i) {
            if (i % 3 == 0)
                s.append(" -----------------------\n");
            for (int j = 0; j < 9; ++j) {
                if (j % 3 == 0) s.append("| ");
                s.append(
                    solution[i][j] == 0
                        ? " "
                        : Integer.toString(solution[i][j])
                );

                s.append(' ');
            }
            s.append("|\n");
        }
        s.append(" ----------------------- ");
        return s.toString();
    }

}
