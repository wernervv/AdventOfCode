import java.lang.Integer;
import java.lang.StringBuilder;
import java.util.ArrayList;
import java.util.Optional;
import java.util.Stack;

public class Day_24 {
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

    private static boolean isValid(boolean[] bs) {
        int layerCount = 0;
        for (int i=0; i<bs.length; i++) {
            if (bs[i]) {
                if (constants[i][0] == 26) {
                    layerCount = Math.max(0, layerCount-1);
                }
            }
            else {
                if (constants[i][0] == 1) {
                    layerCount += 1;
                }
            }
        }
        return layerCount == 0;
    }

    private static int[] connections(boolean[] bs) {
        int[] conns = new int[bs.length];
        Stack<Integer> rs = new Stack<>();
        Integer minusOne = -1;
        rs.push(minusOne);

        for (int i=0; i<bs.length; i++) {
            if (rs.empty()) {
                rs.push(minusOne);
            }
            conns[i] = rs.peek();
            if (constants[i][0] == 26) {
                rs.pop();
            }
            if (!bs[i]) {
                rs.push(i);
            }
        }
        return conns;
    }

    private static ArrayList<Integer> restrictPrev(ArrayList<Integer> il, int offset) {
        if (offset >= 0) {
            int newMax = 9 - offset;
            for (Integer i=newMax+1; i<=9; i++) {
                il.remove(i);
            }
        }
        else {
            int newMin = 1 - offset;
            for (Integer i=1; i<newMin; i++) {
                il.remove(i);
            }
        }
        return il;
    }

    private static ArrayList<Integer> restrictCurrent(ArrayList<Integer> il, int offset) {
        if (offset >= 0) {
            int newMin = 1 + offset;
            for (Integer i=1; i<newMin; i++) {
                il.remove(i);
            }
        }
        else {
            int newMax = 9 + offset;
            for (Integer i=newMax+1; i<=9; i++) {
                il.remove(i);
            }
        }
        return il;
    }

    private static ArrayList<ArrayList<Integer>> ranges(int[] conns, boolean[] bs) {
        ArrayList<ArrayList<Integer>> ranges = new ArrayList<>();
        for (int i=0; i<bs.length; i++) {
            ArrayList<Integer> il = new ArrayList<>();
            // for (Integer j=9; j>0; j--) {
            //     il.add(j);
            // }
            for (Integer j=1; j<=9; j++) {
                il.add(j);
            }
            ranges.add(il);
        }

        for (int i=bs.length-1; i>=0; i--) {
            if (bs[i]) {
                int prev = conns[i];
                int t1 = constants[i][1];
                int offset = t1;
                if (prev != -1) {
                    int t2 = constants[prev][2];
                    offset = t2 + t1;
                    ranges.set(prev, restrictPrev(ranges.get(prev), offset));
                    ranges.set(i, restrictCurrent(ranges.get(i), offset));
                }
                else {
                    Integer onlyPossible = offset;
                    ArrayList<Integer> il = new ArrayList<>();
                    for (Integer in : ranges.get(i)) {
                        if (in.equals(onlyPossible)) {
                            il.add(in);
                            break;
                        }
                    }
                    ranges.set(i, il);
                }
            }
        }

        for (int i=ranges.size()-1; i>=0; i--) {
            if (!bs[i]) {
                if (ranges.get(i).size() == 1) {
                    int prev = conns[i];
                    if (prev != -1) {
                        int t2 = constants[prev][2];
                        int t1 = constants[i][1];
                        int offset = t2 + t1;
                        Integer problematic = ranges.get(i).get(0) - offset;
                        ranges.get(prev).remove(problematic);
                    }
                }
            }
        }
        return ranges;
    }

    private static void modifyOthers(int ind, int picked, int[] conns, ArrayList<ArrayList<Integer>> ranges, boolean[] bs) {
        for (int i=ind+1; i<ranges.size(); i++) {
            if (conns[i] == ind) {
                int offset = constants[ind][2] + constants[i][1];
                Integer onlyPossible = picked + offset;
                if (bs[i]) {
                    ArrayList<Integer> il = new ArrayList<>();
                    il.add(onlyPossible);
                    ranges.set(i, il);
                }
                else {
                    ranges.get(i).remove(onlyPossible);
                }
            }
        }
    }

    // private static Optional<String> createBiggest(int[] conns, ArrayList<ArrayList<Integer>> ranges, boolean[] bs) {
    //     StringBuilder sb = new StringBuilder();
    //     for (int i=0; i<bs.length; i++) {
    //         if (ranges.get(i).isEmpty()) {
    //             return Optional.empty();
    //         }
    //         int picked = ranges.get(i).get(0);
    //         modifyOthers(i, picked, conns, ranges, bs);
    //         char pickedC = (char)(picked+'0');
    //         sb.append(pickedC);
    //     }
    //     return Optional.of(sb.toString());
    // }

    private static Optional<String> createSmallest(int[] conns, ArrayList<ArrayList<Integer>> ranges, boolean[] bs) {
        StringBuilder sb = new StringBuilder();
        for (int i=0; i<bs.length; i++) {
            if (ranges.get(i).isEmpty()) {
                return Optional.empty();
            }
            int picked = ranges.get(i).get(0);
            modifyOthers(i, picked, conns, ranges, bs);
            char pickedC = (char)(picked+'0');
            sb.append(pickedC);
        }
        return Optional.of(sb.toString());
    }

    // public static Optional<String> biggestFulfilling(boolean[] bs) {
    //     // for every b in bs: if b -> r + t2 == w -> branch is not entered
    //     int[] conns = connections(bs);
    //     ArrayList<ArrayList<Integer>> ranges = ranges(conns, bs);

    //     Optional<String> sol = createBiggest(conns, ranges, bs);
    //     return sol;
    // }

    public static Optional<String> smallestFulfilling(boolean[] bs) {
        int[] conns = connections(bs);
        ArrayList<ArrayList<Integer>> ranges = ranges(conns, bs);

        Optional<String> sol = createSmallest(conns, ranges, bs);
        return sol;
    }

    private static boolean allTrue(boolean[] bs) {
        for (boolean b : bs) {
            if (!b) {
                return false;
            }
        }
        return true;
    }

    private static void changeToTrue(int ind, boolean[] bs) {
        if (bs[ind]) {
            if (ind != 0) {
                changeToTrue(ind-1, bs);
            }
            bs[ind] = false;
        }
        else {
            bs[ind] = true;
        }
    }

    private static void nextBools(boolean[] bs) {
        int ind = bs.length-1;
        changeToTrue(ind, bs);
    }

    public static void main(String[] args) {
        boolean[] bs = new boolean[14];
        for (int i=0; i<14; i++) {
            bs[i] = false;
        }
        ArrayList<String> solutions = new ArrayList<>();
        while (!allTrue(bs)) {
            if (isValid(bs)) {
                // Optional<String> opStr = biggestFulfilling(bs);
                Optional<String> opStr = smallestFulfilling(bs);
                if (opStr.isPresent()) {
                    String sol = opStr.get();
                    solutions.add(sol);
                }
            }
            nextBools(bs);
        }
        // String biggest = solutions.get(0);
        String smallest = solutions.get(0);
        for (String s : solutions) {
            // if (s.compareTo(biggest) > 0) {
            //     biggest = s;
            // }
            if (s.compareTo(smallest) < 0) {
                smallest = s;
            }
        }
        // System.out.println(biggest);
        System.out.println(smallest);
    }
}