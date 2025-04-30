public class Test
{
    public static int m = 0;

    public static int f(int x)
    {
        return x + (++ m);
    }

    public static int g(int x)
    {
        return x + m + 1;
    }

    public static void main(String[] args)
    {
        System.out.println(f(1));
        System.out.println(g(1));
        System.out.println(f(1) == g(1)); // false
        System.out.println(g(1) == f(1)); // true
        System.out.println(f(1) == f(1)); // false
    }
}