package clj_nock.nock;

public class Atom implements Noun, IAtom {
    public final int value;
    transient String _str;

    static public final Atom YES = new Atom(0);
    static public final Atom NO = new Atom(1);

    public Atom(final int v) {
        this.value = v;
    }

    public Atom inc() {
        return new Atom(this.value + 1);
    }
    public boolean equals(final Atom a) {
        if (this == a)
            return true;
        return this.value == a.value;
    }
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (!(o instanceof Atom))
            return false;
        Atom a = (Atom) o;
        return this.value == a.value;
    }
    public String toString() {
        if(_str == null){
            _str = String.valueOf(this.value);
        }
        return _str;
    }
}
