package underflow.java;

/**
 * Created by mike on 8/4/14.
 */
public class SafeHarness implements Harness<SafeHarness.HarnessResult> {
	private final Continuation cont;
	private final Object minstance;
	private Boolean obsolete = false;

	public class BoxedContinuation {
		private final Object val;

		private BoxedContinuation(Object val) {
			this.val = val;
		}

		private Object call() {
			return cont.call(SafeHarness.this, val);
		}
	}

	public class HarnessResult {
		private final Object val;

		private HarnessResult(Object val) {
			this.val = val;
		}

		private Object getResult() {
			return val;
		}

		private Object getDict() {
			return SafeHarness.this.getDict();
		}
	}

	public SafeHarness(Continuation cont, Object minstance) {
		this.cont = cont;
		this.minstance = minstance;
	}

	private void markObsolete() {
		//noinspection SynchronizeOnNonFinalField
		synchronized (obsolete) {
			obsolete = true;
		}
	}

	private void checkObsolete() {
		//noinspection SynchronizeOnNonFinalField
		synchronized (obsolete) {
			if (obsolete) {
				throw new IllegalStateException("reuse of modified SafeHarness");
			}
		}
	}

	public Continuation getCont() {
		checkObsolete();
		return cont;
	}

	public SafeHarness setCont(Continuation c) {
		markObsolete();
		return new SafeHarness(c, minstance);
	}

	public Object ret(Object r) {
		checkObsolete();
		if (cont == null) {
			return new HarnessResult(r);
		} else {
			return new BoxedContinuation(r);
		}
	}

	public Object ter(Object r) {
		return ret(r);
	}

	public Object extract(HarnessResult result) {
		return result.getResult();
	}

	public SafeHarness reharness(HarnessResult result) {
		return setDict(result.getDict());
	}

	public Object getDict() {
		checkObsolete();
		return minstance;
	}

	public SafeHarness setDict(Object dict) {
		markObsolete();
		return new SafeHarness(cont, dict);
	}

	public HarnessResult execute(Object input) {
		while (input instanceof BoxedContinuation) {
			input = ((BoxedContinuation) input).call();
		}
		return (HarnessResult)input;
	}
}
