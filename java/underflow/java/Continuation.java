package underflow.java;

/**
 * Created by mike on 8/4/14.
 */
public interface Continuation {
	Object call(Harness h, Object i);
}
