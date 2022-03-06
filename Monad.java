public class Monad {
  private static int[][] constants = {{1,13,8}
                              ,{1,12,13}
                              ,{1,12,8}
                              ,{1,10,10}
                              ,{26,-11,12}
                              ,{26,-13,1}
                              ,{1,15,13}
                              ,{1,10,5}
                              ,{26,-2,10}
                              ,{26,-6,3}
                              ,{1,14,2}
                              ,{26,0,2}
                              ,{26,-15,12}
                              ,{26,-4,7}};

  private static int coreFunction(int round, int z, int w) {
    int[] roundConsts = constants[round];
    int d = roundConsts[0];
    int t1 = roundConsts[1];
    int t2 = roundConsts[2];

    int r = z % 26;
    z = z / d;
    if (r+t1 != w) {
      z = 26*z + w + t2;
    }
    return z;
  }

  public static boolean testNumber(int... digits) {
    int z = 0;
    for (int i=0; i<digits.length; i++) {
      int w = digits[i];
      z = coreFunction(i, z, w);
    }
    if (z == 0) {
      return true;
    }
    else {
      return false;
    }
  }
  private static boolean carryOver(int ind, int[] current) {
    current[ind]--;
    if (current[ind] == 0) {
      current[ind] = 9;
      return true;
    }
    else {
      return false;
    }
  };

  private static int[] oneSmaller(int[] current) {
    boolean carryOver = false;
    int ind = 13;
    while (carryOver && ind >= 0) {
      carryOver = carryOver(ind, current);
    }
    return current;
  }

  public static void main(String[] args) {
    int[] number = {9,9,9,9,9,9,9,9,9,9,9,9,9,9};
    boolean isCorrect = testNumber(number);
    while (!isCorrect) {
      number = oneSmaller(number);
      isCorrect = testNumber(number);
    }
    for (int digit : number) {
      System.out.print(digit);
    }
    System.out.println();
  }
}