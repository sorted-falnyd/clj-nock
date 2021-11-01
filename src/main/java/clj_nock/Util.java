package clj_nock.nock;

public class Util {
    public static Atom loob(final boolean b) {
        if (b) {
            return Atom.YES;
        } else {
            return Atom.NO;
        }
    }
    public static Atom eq(final Noun n1, final Noun n2) {
        return loob(n1.equals(n2));
    }
    public static boolean isAtom(final Noun n) {
        return (n instanceof Atom);
    }
    public static boolean isCell(final Noun n) {
        return (n instanceof Cell);
    }
    public static Atom isCellP(final Noun n) {
        return loob(isCell(n));
    }
    public static boolean isYes(final Noun n) {
        return n == Atom.YES;
    }
    public static Noun axis(final Atom a, final Noun t) {
        int v = a.value;
        if (1 == v) {
            return t;
        }
        else {
            int n = Integer.highestOneBit(v) >>> 1;
            Noun t2 = t;
            while (n != 0) {
                t2 = (((n & v) == 0) ? ((Cell)t2).left : ((Cell)t2).right);
                n --;
            }
            return t2;
        }
    }

}
