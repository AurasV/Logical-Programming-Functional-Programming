public class Factorial {

    public static int state = 0;

    public static int factorial(int x)
    {
        int f = 1;
        while(x > 0)
        {
            f = x * f;
            x = x - 1;
        }
        return f;
    }

    public static void main(String[] args)
    {
        // moment t0 in the program where
        // state = x
        System.out.println(factorial(5)); // 120
        // moment t1 in program, don't have any guarantee
        // about the value of state
    }
}
