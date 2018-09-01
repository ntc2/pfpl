/* Implement unsafe coerce using dubious Java array
 * subtyping. Compiles with no warnings but fails at runtime.

 * In Ch 24 Bob has vague statements about how subtyping is a common
 * area where mistakes are made in real languages. Here's a well known
 * concrete example.
 */

class UnsafeCoerce {
  public static <A,B> B unsafeCoerce(B[] scratch, A a) {
    setHead(scratch, a);
    return scratch[0];
  }

  private static void setHead(Object as[], Object a) {
    as[0] = a;
  }

  public static void main(String args[]) {
    String s = "A String";
    Integer scratch[] = { null };
    Integer i = unsafeCoerce(scratch, s);
    System.out.println(i.toString());
  }
}
