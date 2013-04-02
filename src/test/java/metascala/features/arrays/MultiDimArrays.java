package metascala.features.arrays;

public class MultiDimArrays {
    public static int[][] make2D(int a, int b){
        return new int[a][b];
    }
    public static int[][][] make3D(int a, int b, int c){
        return new int[a][b][c];
    }
    public static int getAndSet(){
        int[][] arr = new int[10][10];
        for(int i = 0; i < 10; i++){
            for(int j = 0; j < 10; j++){
                arr[i][j] = i + j;
            }
        }
        int total = 0;
        for(int i = 0; i < 10; i++){
            for(int j = 0; j < 10; j++){
                total += arr[i][j];
            }
        }
        return total;
    }

}
