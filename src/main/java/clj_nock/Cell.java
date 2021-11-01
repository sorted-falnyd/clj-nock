package clj_nock.nock;

public class Cell implements Noun {
    public final Noun left;
    public final Noun right;
    transient String _str;

    public Cell(final Noun left, final Noun right) {
        this.left = left;
        this.right = right;
    }

    public boolean equals(final Cell c) {
        if (this == c)
            return true;
        return this.left.equals(c.left) && this.right.equals(c.right);
    }

    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (!(o instanceof  Cell))
            return false;
        Cell c = (Cell) o;
        return this.left.equals(c.left) && this.right.equals(c.right);
    }

    public String toString() {
        if(_str == null){
            _str = "[" + left.toString() + " " + right.toString() + "]";
        }
        return _str;
    }
}
