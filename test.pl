test(is_a_list) :-
    %positiv
    write('is_a_list +'),
    is_a_list([]), write('.'),!,
    is_a_list([1]), write('.'),!,
    is_a_list([1, 2]), write('.'),!,
    is_a_list([1, 2,[a,[b,[c],d]]]),
    %negativ
    write('. -'),!,
    \+ is_a_list([kopf|rest]), write('.'),!,
    \+ is_a_list((kopf,rest)), write('.'),!,
    \+ is_a_list((_Kopf,_Rest)), write('.'),!,
    \+ is_a_list(liste),!, writeln('ok').

test(diffList) :-
    write('diffList'),
    diffList([a,b,c,d], [b,d,f], [a, c]), write('.'),!,
    diffList([a,b,c,d], [], [a, b, c, d]), write('.'),!,
    diffList([a,b,c,d], [d,c,b,a], [ ]), write('.'),!,
    diffList([a,b,c,d,e], [d,1,f,e,2], [a, b, c]),
    %negativ
    write('. -'),!,
    \+ diffList([a,b,c,d], [d,c,b,a], [a,b,c,d]), write('.'),!,
    \+ diffList([a,b,c,d], [b,d,f], [ ]), write('.'),!,
    \+ diffList([a,b,c,d], [ ], [ ]),!, writeln('ok').

test(infix) :-
    %positiv
    write('infix +'),
    infix([2,3], [1, 2, 3, 4]), write('.'),!,
    infix([2,3], [1, 2, 9, 2, 3, 4]), write('.'),!,
    infix([2,3], [1, 3, 9, 2, 3, 4]),
    %negativ
    write('. -'),!,
    \+ infix([], [1, 2, 3, 4]), write('.'),!,
    \+ infix([1,2], [1, 2, 3, 4]), write('.'),!,
    \+ infix([3,4], [1, 2, 3, 4]), write('.'),!,
    \+ infix([4], [1, 2, 3, 4]), write('.'),!,
    \+ infix([2,3], [1, 2, 5, 6, 3, 4]), write('.'),!,
    \+ infix([2, 3], [1, [2, 3],4]),!, writeln('ok').

test(suffix) :-
    %positiv
    write('suffix +'),
    suffix([3], [1, 2, 3]), write('.'),!,
    suffix([2, 3], [1, 2, 3]), write('.'),
    %negativ
    write('. -'),!,
    \+ suffix([], [1, 2, 3]), write('.'),!,
    \+ suffix([1, 2, 3], [1, 2, 3]), write('.'),!,
    \+ suffix([2, 3], [1, 2, 4, 3]), write('.'),!,
    \+ suffix([2, 3], [1, [2, 3]]),!, writeln('ok').

test(praefix) :-
    %positiv
    write('praefix +'),
    praefix([1], [1, 2, 3]), write('.'),!,
    praefix([1, 2], [1, 2, 3]), write('.'),
    %negativ
    write('. -'),!,
    \+ praefix([], [1, 2, 3]), write('.'),!,
    \+ praefix([1, 2, 3], [1, 2, 3]), write('.'),
    \+ praefix([1, 2], [1, 4, 2, 3]), write('.'),!,
    \+ praefix([1, 2], [[1, 2], 3]),!, writeln('ok').

test(eo_count) :-
    write('eo_count'),
    eo_count([],1,0), write('.'),!,
    eo_count([1,2],1,0), write('.'),!,
    eo_count([1],0,1), write('.'),!,
    eo_count([1,[],[1],4,[1,2],[1,2,3],7],2,3), write('.'),!,
    eo_count([1,[1,[],[1],4,[1,2],[1,2,3],7],[1],4,[1,2],[1,2,3],7],3,6),!, writeln('ok').

test(del_element) :-
    write('del_element'),
    del_element(l,1, [1, 2, 3, 1, 4, 5, 1, 1], [1, 2, 3, 1, 4, 5, 1]), write('.'),!,
    del_element(e,1, [1, 2, 3, 1, 4, 5, 1, 1], [2, 3, 1, 4, 5, 1, 1]), write('.'),!,
    del_element(a,1, [1, 2, 3, 1, 4, 5, 1, 1], [2, 3, 4, 5]),!, writeln('ok').

test(substitute) :-
    write('substitute'),
    substitute(l,1,x, [1, 2, 3, 1, 4, 5, 1, 1], [1, 2, 3, 1, 4, 5, 1, x]), write('.'),!,
    substitute(e,1,x, [1, 2, 3, 1, 4, 5, 1, 1], [x, 2, 3, 1, 4, 5, 1, 1]), write('.'),!,
    substitute(a,1,x, [1, 2, 3, 1, 4, 5, 1, 1], [x, 2, 3, x, 4, 5, x, x]),!, writeln('ok').

test(alllist) :-
    test(is_a_list),
    test(diffList),
    test(infix),
    test(suffix),
    test(praefix),
    test(eo_count),
    test(del_element),
    test(substitute)
    .

test(nat) :-
    %positiv
    write('nat +'),
    nat(0), write('.'),!,
    nat(s(0)), write('.'),!,
    nat(s(s(0))),
    %negativ
    write('. -'),!,
    \+ nat(42), write('.'),!,
    \+ nat(null), write('.'),!,
    \+ nat(s(s(1))),!, writeln('ok').

test(nat2s) :-
    write('nat2s'),
    nat2s(0, 0), write('.'),!,
    nat2s(1, s(0)), write('.'),!,
    nat2s(2, s(s(0))), write('.'),!,
    nat2s(3, s(s(s(0)))),!, writeln('ok').

test(s2nat) :-
    write('s2nat'),
    s2nat(0, 0), write('.'),!,
    s2nat(s(0),1), write('.'),!,
    s2nat(s(s(0)),2), write('.'),!,
    s2nat(s(s(s(s(s(s(s(s(0)))))))),8),!, writeln('ok').

test(add) :-
    write('add'),
    add(0, 0, 0), write('.'),!,
    add(s(0), 0, s(0)), write('.'),!,
    add(0,s(0), s(0)), write('.'),!,
    add(s(0), s(0), s(s(0))), write('.'),!,
    add(s(0), s(s(0)), s(s(s(0)))), write('.'),!,
    add(s(s(s(0))), s(s(s(s(0)))), s(s(s(s(s(s(s(0)))))))),!, writeln('ok').

test(sub) :-
    write('sub'),
    sub(0, 0, 0), write('.'),!,
    sub(s(s(0)), s(0), s(0)), write('.'),!,
    sub(s(s(s(s(s(s(s(0))))))), s(s(s(s(s(s(s(0))))))), 0), write('.'),!,
    sub(s(0), s(s(s(s(s(s(s(0))))))), 0), write('.'),!,
    sub(s(s(s(s(s(s(s(0))))))), s(s(s(s(0)))),s(s(s(0)))),!, writeln('ok').

test(mul) :-
    write('mul'),
    mul(0,s(s(s(0))), 0), write('.'),!,
    mul(s(s(0)), 0, 0), write('.'),!,
    mul(s(s(0)), s(s(s(0))), s(s(s(s(s(s(0))))))), write('.'),!,
    mul(s(s(0)), s(0), s(s(0))), write('.'),!,
    mul(s(0), s(0), s(0)),!, writeln('ok').

test(power) :-
    write('power'),
    power(0, 0, s(0)), write('.'),!,
    power(s(s(s(s(0)))), 0, s(0)), write('.'),!,
    power(0, s(0), 0), write('.'),!,
    power(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(0)))))))))))))))), s(0), s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(0))))))))))))))))), write('.'),!,
    power(s(0), s(s(0)), s(0)), write('.'),!,
    power(s(s(0)), s(s(0)), s(s(s(s(0))))), write('.'),!,
    power(s(s(0)), s(s(s(0))), s(s(s(s(s(s(s(s(0))))))))), write('.'),!,
    power(s(s(s(s(0)))), s(s(0)), s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(0))))))))))))))))),!, writeln('ok').

test(fac) :-
    write('fac'),
    fac(0, s(0)), write('.'),!,
    fac(s(0), s(0)), write('.'),!,
    fac(s(s(0)), s(s(0))), write('.'),!,
    fac(s(s(s(s(0)))), s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(0))))))))))))))))))))))))),!, writeln('ok').

test(lt) :-
    %positiv
    write('lt +'),
    lt(0, s(0)), write('.'),!,
    lt(s(s(0)), s(s(s(s(s(s(s(s(0))))))))), write('.'),!,
    %negativ
    write('. -'),!,
    \+ lt(0, 0), write('.'),!,
    \+ lt(s(s(s(s(s(s(s(s(0)))))))), s(s(s(s(s(s(s(s(0))))))))), write('.'),!,
    \+ lt(s(s(s(s(s(s(s(s(0)))))))), s(s(0))),!, writeln('ok').

test(mods) :-
    write('mods'),
    mods(s(s(s(s(s(s(s(s(0)))))))), s(0), 0), write('.'),!,
    mods(s(s(s(s(s(0))))), s(s(s(s(s(0))))), 0), write('.'),!,
    mods(s(s(s(s(s(s(s(s(0)))))))), s(s(s(s(s(0))))), s(s(s(0)))), write('.'),!,
    mods(s(s(s(0))), s(s(s(s(s(0))))), s(s(s(0)))), write('.'),!,
    %negativ
    write('. -'),!,
    \+ mods(s(s(s(0))), 0, _Something),!, writeln('ok').

test(allnat) :-
    test(nat),
    test(nat2s),
    test(s2nat),
    test(add),
    test(sub),
    test(mul),
    test(power),
    test(fac),
    test(lt),
    test(mods)
    .
 % Druckt eine Liste in Zeilen aus
ppList([Elem|Rest]) :- writeln(Elem),ppList(Rest).
ppList([]).



power(_,0,s(0)).
power(Basis,s(RExp),Erg) :-
       power(Basis,RExp,Basis,Erg).
power(_Basis,0,Result,Result).
power(Basis,s(RExp),Result,Erg) :-
       mul(Result,Basis,NewResult),
       power(Basis,RExp,NewResult,Erg).