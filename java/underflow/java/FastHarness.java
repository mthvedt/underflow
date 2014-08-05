package underflow.java;

/**
 * Created by mike on 8/4/14.
 */
public class FastHarness implements Harness<Object> {
	private Continuation cont;
	private Object minstance;

	public FastHarness(Continuation cont, Object minstance) {
		this.cont = cont;
		this.minstance = minstance;
	}

	public Continuation getCont() {
		return cont;
	}

	public FastHarness setCont(Continuation c) {
		this.cont = c;
		return this;
	}

	public Object ret(Object r) {
		return r;
	}

	public Object ter(Object r) {
		if (cont == null) {
			return r;
		} else {
			return cont.call(this, r);
		}
	}

	public Object extract(Object o) {
		return o;
	}

	public FastHarness reharness(Object o) {
		return this;
	}

	public Object getDict() {
		return minstance;
	}

	public FastHarness setDict(Object dict) {
		this.minstance = dict;
		return this;
	}

	public Object execute(Object input) {
		while (cont != null) {
			input = cont.call(this, input);
		}
		return input;
	}
}
