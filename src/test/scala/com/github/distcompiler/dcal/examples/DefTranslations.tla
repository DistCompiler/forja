-------------------------- MODULE DefTranslations --------------------------
EXTENDS Integers, FiniteSets

\* DCal: def mt () {}
mt(_state1) ==
    _state1

\* DCal: def resetString() { str := "new string" }
resetString(_state1) ==
    LET
        _state2 == { [s EXCEPT !.str = "new string"]: s \in _state1 }
    IN
        _state2

\* DCal: def baz() { y := y - 1 || x := x + 1 }
baz(_state1) ==
    LET
        _state2 == { [s EXCEPT !.y = s.y - 1, !.x = s.x + 1]: s \in _state1 }
    IN
        _state2

\* DCal: def sum(p1, p2) { let local = p1 + p2 x := local }
sum(_state1, p1, p2) ==
    LET
        _state2 == UNION {
            LET
                local == p1 + p2
            IN
                { [ss EXCEPT !.x = local] : ss \in { s } }
        : s \in _state1 }
    IN
        _state2

\* def foo() { let z == y + 1 x := z - 3 await x < 7 }
foo(_state1) ==
    LET
        \* UNION is for flattening the sets produced by nested let expressions
        _state2 == UNION {
            LET
                z == s.y + 1
            IN
                LET
                    _state3 == { [ss EXCEPT !.x = z - 3 ]: ss \in { s } }
                IN
                    LET
                        _state4 == { ss \in _state3: ss.x < 7 }
                    IN
                        _state4
        : s \in _state1 }
    IN
        _state2

\* DCal: def branch1() { if x <= y then { x := x + 1 } else { y := y - 1 } }
\* where x, y are state member variables
branch1(_state1) ==
    LET
        _state2 == UNION {
            IF s.x <= s.y
            THEN
                LET _state3 == { [ss EXCEPT !.x = ss.x + 1]: ss \in { s } }
                IN _state3
            ELSE
                LET _state3 == { [ss EXCEPT !.y = ss.y - 1]: ss \in { s } }
                IN _state3
        : s \in _state1 }
    IN
        _state2

\* DCal: def branch2() { if x <= y then { i := i * x x := x + 1 } else { y := y - 1 } }
branch2(_state1) ==
    LET
        _state2 == UNION {
            IF s.x <= s.y
            THEN
                LET _state3 == { [ss EXCEPT !.i = ss.i * ss.x]: ss \in { s } }
                IN
                    LET _state4 == { [sss EXCEPT !.x = sss.x + 1]: sss \in _state3 }
                    IN _state4
            ELSE
                LET _state3 == { [ss EXCEPT !.y = ss.y - 1]: ss \in { s } }
                IN _state3
        : s \in _state1 }
    IN
        _state2

\* DCal: def branchOnLocal() { let b = TRUE if b then { x := x + 1 } else { y := y - 1 } }
\* where b is a local
branchOnLocal(_state1) ==
    LET
        _state2 == UNION {
            LET
                b == TRUE
            IN
                LET
                    \* if b then { x := x + 1 } else { y := y - 1 }
                    _state3 == { IF b
                                 THEN [ss EXCEPT !.x = ss.x + 1]
                                 ELSE [ss EXCEPT !.y = ss.y - 1]  : ss \in { s } }
                IN
                    _state3
            : s \in _state1
        }
    IN
        _state2

\* DCal: def bar(v) { y := y - v i := i + 1 }
bar(_state1, v) ==
    LET
        _state2 == { [s EXCEPT !.y = s.y - v ]: s \in _state1 }
    IN
        LET
            _state3 == { [s EXCEPT !.i = s.i + 1]: s \in _state2 }
        IN
            _state3

(* Test expressions
states == {
    [id |-> 1, x |-> 0, y |-> 0, str |-> "", i |-> 1],
    [id |-> 2, x |-> 10, y |-> 10, str |-> "", i |-> 1],
    [id |-> 3, x |-> -10, y |-> -10, str |-> "", i |-> 1]
}

resetString(states) ->
{
    [id |-> 1, x |-> 0, y |-> 0, str |-> "new string", i |-> 1],
    [id |-> 2, x |-> 10, y |-> 10, str |-> "new string", i |-> 1],
    [id |-> 3, x |-> -10, y |-> -10, str |-> "new string", i |-> 1]
}

sum(states, 3, 7) ->
{
    [id |-> 1, x |-> 10, y |-> 0, str |-> "", i |-> 1],
    [id |-> 2, x |-> 10, y |-> 10, str |-> "", i |-> 1],
    [id |-> 3, x |-> 10, y |-> -10, str |-> "", i |-> 1]
}

foo(states) ->
{
    [id |-> 1, x |-> -2, y |-> 0, str |-> "", i |-> 1],
    [id |-> 3, x |-> -12, y |-> -10, str |-> "", i |-> 1]
}

bar(states, 5) ->
{
    [id |-> 1, x |-> 0, y |-> -5, str |-> "", i |-> 1],
    [id |-> 2, x |-> 10, y |-> 5, str |-> "", i |-> 1],
    [id |-> 3, x |-> -10, y |-> -15, str |-> "", i |-> 1]
}
*)
=============================================================================