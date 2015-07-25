use "assignment3.sml";

val only_capitals = ["Dog"]=(only_capitals ["Dog", "dog"])

fun run_test test = if test
		    then print "PASS\n"
		    else print "FAIL\n"

val test1 = run_test (1=count_wildcards Wildcard)
val test2 = run_test (3=count_wildcards (TupleP [Wildcard, Wildcard, Variable "hello", Wildcard]))
