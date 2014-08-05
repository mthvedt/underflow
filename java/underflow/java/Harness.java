package underflow.java;

/**
 * Created by mike on 8/4/14.
 */
public interface Harness<R> {
	Continuation getCont();
	Harness<R> setCont(Continuation c);
	Object ret(Object r);
	Object ter(Object r);
	Object extract(R r);
	Harness<R> reharness(R r);
	Object getDict();
	Harness<R> setDict(Object dict);
	R execute(Object input);
}