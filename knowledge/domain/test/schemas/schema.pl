% schema database test
% @author Ivo Swartjes
% @date 29 oktober 2007

:- multifile action_schema/1.

action_schema([
	type(test),
    arguments([agens(Agens)]),
	duration(1),
	preconditions([
        condition(true, [
            fact(Agens, b, c),
			fabula(d, e, f),
			fabula(g, h, i, j),
			rule(d1, e1, f1)
		]),
		condition(false, [
            fact(Agens, k, l),
			fabula(m, n, o),
			fabula(p, q, r, s),
			rule(t, u, v)
		])
	]),
	effects([
		condition(true, [
		    fact(Agens,	b, c)
		]),
        condition(false, [
            fact(Agens, d, e),
            fact(f, g, h)
        ])
    ])
]).


