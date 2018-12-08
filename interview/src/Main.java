import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;

public class Main {
    /**
     * Iterate through each line of input.
     */
    public static void main(String[] args) throws IOException {
        InputStreamReader reader = new InputStreamReader(System.in, StandardCharsets.UTF_8);
        BufferedReader in = new BufferedReader(reader);
        String line;
        int iterNumber = 0;
        while ((line = in.readLine()) != null) {
            Integer palindrome1 = Integer.parseInt(new Main().getPalindrome(line));
            Integer temp = Integer.parseInt(line) + palindrome1;
            new Main().helper(1, temp.toString());

        }
    }

        public String helper(int iterNum,String line) {
            if (isPalindrome(line)){
                System.out.print(iterNum);
                System.out.print(" ");
                System.out.print(line);
                return("");

            }else{
                Integer number = Integer.parseInt(line) + Integer.parseInt(getPalindrome(line));
                return helper(iterNum+1, number.toString());
            }
        }

    public boolean isPalindrome(String line) {
        Boolean bool = false;
        String temp = getPalindrome(line);

        if (line.equals(temp)) {
            bool = true;
        }
        return bool;
    }

    public String getPalindrome(String line) {
        String temp = "";
        char[] arr = line.toCharArray();
        for (int i = arr.length - 1; i >= 0; i--) {
            temp += arr[i];
        }
        return temp;
    }


}




