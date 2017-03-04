/* Bartosz Sojka Pelny język.*/


algol16(Source, SextiumBin) :-
	lexed(Source, TokenList),
	parsed(TokenList, ADR),
	namespace_converted(ADR, NamedADR),
	compiled(NamedADR, HighLevelAssemblerADR),
	low_leveled(HighLevelAssemblerADR, SextiumMachineCodeBlocks),
	flated(SextiumMachineCodeBlocks, SextiumMachineCode),
	binarized(SextiumMachineCode, SextiumBin), change_format(SextiumBin, SextiumHex),
	open('SextiumBin', write, Stream),
	write(Stream, SextiumHex),
	nl(Stream),
	close(Stream).


change_format([],[]):-!.
change_format([H|T], [G|S]) :- format(atom(G), '~|~`0t~16r~4+~n', H), change_format(T, S).

:- use_module(library(dcg/basics)).

lexed(Source, TokenList) :- phrase(lexer(TokenList), Source).

lexer(TokenList) --> 
	pomin,
	(	( "+",  !, {Token = tokPlus}
		; "-",  !, {Token = tokMinus}
		; "*",  !, {Token = tokAsteriks}
		; "<>", !, {Token = tokNeq}
		; "<=", !, {Token = tokLeq}
		; "<",  !, {Token = tokLt}
		; ">=", !, {Token = tokGeq}
		; ">",  !, {Token = tokGt}
		; "=",  !, {Token = tokEq}
		; ":=", !, {Token = tokAssign}
		; "(",  !, {Token = tokLPar}
		; ")",  !, {Token = tokRPar}
		; ";",  !, {Token = tokSColon}
		; ",",  !, {Token = tokComma}
		;	digit(D), !,  number(D, N), {Token = tokNumber(N)}
		; letter(L), !, identifier(L, Id),
			{ member( (Id, Token), [    (and, tokAnd),
										(begin, tokBegin),
										(call, tokCall),
										(div, tokDiv),
										(do, tokDo),
										(done, tokDone),
										(else, tokElse),
										(end, tokEnd),
										(fi, tokFi),
										(if, tokIf),
										(local, tokLocal),
										(mod, tokMod),
										(not, tokNot),
										(or, tokOr),
										(procedure, tokProcedure),
										(program, tokProgram),
										(read, tokRead),
										(return, tokReturn),
										(then, tokThen),
										(value, tokValue),
										(while, tokWhile),
										(write, tokWrite)  ] ), !
		;	Token = tokName(Id) }
		; [_], {Token = tokUnknown}
		), !, {TokenList = [Token|TokList] }, lexer(TokList)
		; [], {TokenList = []}
	).

pomin --> (comments, pomin) ; (blank, pomin), !.
pomin --> [].

comments --> "(*", symbols.
symbols --> "*)".
symbols --> [_], symbols.

number(D, N) --> digits(Ds), {number_chars(N, [D|Ds]) }.

letter(L) --> [L], {code_type(L, alpha) }.

alphanum([A|T]) --> [A], ({code_type(A, alnum) } ; {[A] == "_"} ; {[A] == "'"} ), !, alphanum(T).
alphanum([]) --> [].

identifier(L, Id) --> alphanum(As), {atom_codes(Id, [L|As]) }.













parsed(TokenList, ADR) :- phrase(parse(ADR), TokenList).

parse(ADR) --> program(ProgramADR), {ADR = program(ProgramADR)}.

program(ADR) --> [tokProgram], [tokName(_)], blok(Deklaracje, Instzl), {ADR = blok(Deklaracje, Instzl)}.

blok(DeklaracjePred, InstzlPred) --> deklaracje(Deklaracje), [tokBegin], !, instrukcja_zlozona(Instzl), [tokEnd], {DeklaracjePred = deklarations_obj(Deklaracje), InstzlPred = instrukcja_zlozona(Instzl)}.

deklaracje([H|T]) --> deklaracja(H), !, deklaracje(T).
deklaracje([]) --> [].

deklaracja(Deklaracja) --> deklarator(Deklaracja), ! ; procedura(Nazwa, Argumenty, Blok), !, {Deklaracja = procedura(Nazwa, Argumenty, Blok)}.

deklarator(Deklarator) --> [tokLocal], zmienne(Vars), {Deklarator = vars_obj(Vars)}.

zmienne([H|T]) --> zmienna(H), [tokComma], !, zmienne(T).
zmienne([A]) --> zmienna(A).

zmienna(Zmienna) --> [tokName(Id)], {Zmienna = var_obj(Id)}.

procedura(Nazwa, Argumenty, Blok) --> [tokProcedure], [tokName(Nazwa)], [tokLPar], argumenty_formalne(Argumenty), [tokRPar], blok(Deklaracje, Instzl), {Blok = blok(Deklaracje, Instzl)}.

argumenty_formalne([H|T]) --> argument_formalny(H), [tokComma], !, argumenty_formalne(T).
argumenty_formalne([A]) --> argument_formalny(A), !.
argumenty_formalne([]) --> [].

argument_formalny(Argument) --> [tokName(Name)], !, {Argument = arg_obj([name, Name])} ; [tokValue], [tokName(Name)], {Argument = arg_obj([value, Name])}.

instrukcja_zlozona([H|T]) --> instrukcja(H), [tokSColon], !, instrukcja_zlozona(T).
instrukcja_zlozona([A]) --> instrukcja(A).

instrukcja(Instrukcja) --> 
		[tokName(Zmienna)], !, [tokAssign], wyrazenie_arytmetyczne(Wyrazenie), {Instrukcja = assign(var_obj(Zmienna), Wyrazenie)}

	;	[tokIf], bool_exp(Bool), [tokThen], instrukcja_zlozona(InstzlThen), [tokElse], !, instrukcja_zlozona(InstzlElse), [tokFi], {Instrukcja = if(Bool, InstzlThen, InstzlElse)}

	;	[tokIf], !, bool_exp(Bool), [tokThen], instrukcja_zlozona(Instzl), [tokFi], {Instrukcja = if(Bool, Instzl)}

	;	[tokWhile], !, bool_exp(Bool), [tokDo], instrukcja_zlozona(Instzl), [tokDone], {Instrukcja = while(Bool, Instzl)}

	;	[tokCall], !, wywolanie_procedury(Procedure_Name, Argumenty), {Instrukcja = call_acc(Procedure_Name, Argumenty)}

	;	[tokReturn], !, wyrazenie_arytmetyczne(Wyrazenie), {Instrukcja = return(Wyrazenie)}

	;	[tokRead], !, [tokName(Id)], {Instrukcja = read_acc(var_obj(Id))}

	;	[tokWrite], wyrazenie_arytmetyczne(Wyrazenie), {Instrukcja = write_acc(Wyrazenie)}.

	

wyrazenie_arytmetyczne(ONP) --> skladnik(Skladnik), wyrazenie_arytmetyczne(Skladnik, Wyrazenie), {flatten(Wyrazenie, ONP)}.

wyrazenie_arytmetyczne(Acc, Wyrazenie) --> 
		add_op(Op), !, skladnik(Skladnik), {Acc1 = [Acc, Skladnik, Op]}, wyrazenie_arytmetyczne(Acc1, Wyrazenie).
wyrazenie_arytmetyczne(Acc, Acc) --> [].

skladnik(Skladnik) --> czynnik(Czynnik), skladnik(Czynnik, Skladnik).

skladnik(Acc, Skladnik) --> 
		mult_op(Op), !, czynnik(Czynnik), {Acc1 = [Acc, Czynnik, Op]}, skladnik(Acc1, Skladnik).
skladnik(Acc, Acc) --> [].

czynnik(Czynnik) --> [tokMinus], !, simple_exp(SimpleExp), {Czynnik = [SimpleExp, opp]} ; simple_exp(Czynnik).

simple_exp(Expression) --> [tokLPar], !, wyrazenie_arytmetyczne(Expression), [tokRPar] ; wyrazenie_atomowe(Expression).

wyrazenie_atomowe(Wyrazenie) --> 
		wywolanie_procedury(ProcedureName, Argumenty), !, {Wyrazenie = vcall_acc(ProcedureName, Argumenty)}
	;	[tokName(Id)], {Wyrazenie = var_obj(Id)}, ! 
	;	[tokNumber(Id)], {Wyrazenie = num_obj(Id)}.

add_op(add_acc) --> [tokPlus], !.
add_op(substract_acc) --> [tokMinus].

mult_op(mult_acc) --> [tokAsteriks], !.
mult_op(div_acc) --> [tokDiv], !.
mult_op(mod_acc) --> [tokMod].


wywolanie_procedury(Procedure_Name, Argumenty) --> [tokName(Procedure_Name)], [tokLPar], argumenty_faktyczne(Argumenty), [tokRPar].

argumenty_faktyczne([H|T]) --> argument_faktyczny(H), [tokComma], !, argumenty_faktyczne(T).
argumenty_faktyczne([A]) --> argument_faktyczny(A), !.
argumenty_faktyczne([]) --> [].

argument_faktyczny(Arg) --> wyrazenie_arytmetyczne(Arg).

bool_exp(ONP) --> koniunkcja(Koniunkcja), bool_exp(Koniunkcja, Bool), {flatten(Bool, ONP)}.

bool_exp(Acc, Bool) -->
		[tokOr], !, koniunkcja(Koniunkcja), {Acc1 = [Acc, Koniunkcja, or_dep]}, bool_exp(Acc1, Bool).
bool_exp(Acc, Acc) --> [].

koniunkcja(Koniunkcja) --> warunek(War), koniunkcja(War, Koniunkcja).

koniunkcja(Acc, Koniunkcja) -->
		[tokAnd], !, warunek(War), {Acc1 = [Acc, War, and_dep]}, koniunkcja(Acc1, Koniunkcja).
koniunkcja(Acc, Acc) --> [].


warunek(War) -->
		[tokNot], !, wyrazenie_relacyjne(Wyr), {War = [Wyr, not_dep]}
	;	wyrazenie_relacyjne(War).

wyrazenie_relacyjne(Wyr) -->
		[tokLPar], bool_exp(Wyr), !, [tokRPar]
	;	wyrazenie_arytmetyczne(Arytm1), rel_op(Op), wyrazenie_arytmetyczne(Arytm2), {Wyr = [Arytm1, Arytm2, Op]}.

rel_op(less_rel) --> [tokLt], !.
rel_op(lesseq_rel) --> [tokLeq], !.
rel_op(gr_rel) --> [tokGt], !.
rel_op(greq_rel) --> [tokGeq], !.
rel_op(eq_rel) --> [tokEq], !.
rel_op(noteq_rel) --> [tokNeq].

	
	
namespace_converted(ADR, NamedADR) :- labeled(ADR, [], [], NamedADR1, _, _), thunk_checked(NamedADR1, NamedADR).


labeled(blok(deklarations_obj(Deklaracje),instrukcja_zlozona(Instzl)), List, PList, blok(deklarations_obj(Deks), instrukcja_zlozona(IsNSrc)), VarArgList, ProcedureList) :- !,
							filled_list(List, VarArgList, Deklaracje, 0, PList, ProcedureList, Deks),
							named_instructions(Instzl, VarArgList, ProcedureList, IsNS),
							return_check(IsNS, IsNSrc).
							
labeled(program(T), List, PList, program(NA, LZP), VarArgList, ProcedureList) :- licz_zmienne(T, LZP), labeled(T, List, PList, NA, VarArgList, ProcedureList).


filled_list(Acc, Acc, [], _, Acc2, Acc2, []) :- !.
filled_list(Acc, VarArgList, [H|T], N, Acc2, ProcedureList, [S|G]) :- 
								vars_obj(A) = H, !, dodane(Acc, AccAdded, A, N), length(A, L), N1 is N + L, S = H, filled_list(AccAdded, VarArgList, T, N1, Acc2, ProcedureList, G)
							
							;	H = procedura(Name, Argumenty, Blok), 
								licz_zmienne(Blok, LZ),
								length(Argumenty, LArg),
								A = [0, Name, Adres, LArg, LZ, NameValueMap],
								dodane_arg(Name, Acc, AccAdd, Argumenty, 0, NameValueMap), 
								one_up(AccAdd, AccAddUp),
								one_up([A|Acc2], Acc2Up),
								sort(AccAddUp, AccAddUpSort),
								sort(Acc2Up, Acc2UpSort),
								labeled(Blok, AccAddUpSort, Acc2UpSort, LBlok, _, _),  
								S = procedura(Name, Argumenty, LBlok, Adres, LArg, LZ, NameValueMap), 
								filled_list(Acc, VarArgList, T, N, [A|Acc2], ProcedureList, G).
						

one_up([], []) :- !.
one_up([H|T], [G|S]) :- [N|ABC] = H, N2 is N + 1, G = [N2|ABC], one_up(T, S).
							

dodane(Acc, Acc, [], _) :- !.
dodane(Acc, AccAdded, [H|T], N) :- var_obj(Name) = H, A = [0, Name, N, var_obj], N2 is N + 1, dodane([A|Acc], AccAdded, T, N2).
			
dodane_arg(_, Acc, Acc, [], _, []) :- !.
dodane_arg(PName, Acc, AccAdded, [H|T], N1, [S|G]) :- arg_obj([NV, Name]) = H, A = [-1, Name, N1, arg_obj, NV, PName], S=NV, N2 is N1 + 1, dodane_arg(PName, [A|Acc], AccAdded, T, N2, G).
			
licz_zmienne(blok(deklarations_obj(Deklaracje),_), N) :- licz_zmienne_l(Deklaracje, 0, N).

licz_zmienne_l([], N, N) :- !.

licz_zmienne_l([H|T], Acc, N) :- H = vars_obj(A), !, length(A, L), Acc2 is Acc + L, licz_zmienne_l(T, Acc2, N)
					;	licz_zmienne_l(T, Acc, N).


return_check(Is, Is) :- last(Is, return(_)), !.
return_check(Is, IsRa) :- append(Is, [return([num_obj(0)])], IsRa).

named_instructions([], _, _, []) :- !.
named_instructions([H|T], VAL, PV, [G|S]) :-
				H = assign(var_obj(A), B), !, (member([N, A, N2, var_obj], VAL), G = assign(var_obj([N, A, N2]), B2), ! ; member([N, A, N2, arg_obj, NV, PName], VAL), G = assign(arg_obj([N, A, N2, NV, PName]), B2) ), n_b_wyr(B, VAL, PV, B2), named_instructions(T, VAL, PV, S)
			
			;	H = if(Bool, InstzlThen, InstzlElse), !, G = if(Bool2, I2, I22), n_b_wyr(Bool, VAL, PV, Bool2), named_instructions(InstzlThen, VAL, PV, I2), named_instructions(InstzlElse, VAL, PV, I22), named_instructions(T, VAL, PV, S)
				
			;	H = if(Bool, Instzl), !, G = if(Bool2, Instzl2), n_b_wyr(Bool, VAL, PV, Bool2), named_instructions(Instzl, VAL, PV, Instzl2), named_instructions(T, VAL, PV, S)
			
			;	H = while(Bool, Iz), !, G = while(Bool2, Iz2), n_b_wyr(Bool, VAL, PV, Bool2), named_instructions(Iz, VAL, PV, Iz2), named_instructions(T, VAL, PV, S)
			
			;	H = call_acc(PN, A), !, G = call_acc(PN2, A2), member([N, PN, Adres, LArg, LZ, NVM], PV), PN2 = [N, PN, Adres, LArg, LZ, NVM], n_l(A, VAL, PV, A2), named_instructions(T, VAL, PV, S)
			
			;	H = return(W), !, G = return(W2), n_b_wyr(W, VAL, PV, W2), named_instructions(T, VAL, PV, S)
			
			;	H = read_acc(var_obj(A)), !, (member([N, A, N2, var_obj], VAL), G = read_acc(var_obj([N, A, N2])), ! ; member([N, A, N2, arg_obj, NV, PName], VAL), G = read_acc(arg_obj([N, A, N2, NV, PName])) ), named_instructions(T, VAL, PV, S)
			
			;	H = write_acc(W), G = write_acc(W2), n_b_wyr(W, VAL, PV, W2), named_instructions(T, VAL, PV, S).
			
			

n_l([], _, _, []) :- !.
n_l([H|T], VAL, PV, [G|S]) :- n_b_wyr(H, VAL, PV, G), n_l(T, VAL, PV, S). 



n_b_wyr([], _, _, []) :- !. 
n_b_wyr([H|T], VAL, PV, [S|G]) :-
							var_obj(A) = H, !, (member([N, A, N2, var_obj], VAL), S = var_obj([N, A, N2]), ! ; member([N, A, N2, arg_obj, NV, PName], VAL), S = arg_obj([N, A, N2, NV, PName]) ), n_b_wyr(T, VAL, PV, G)
						;	vcall_acc(PName, Arg) = H, !, member([N, PName, Adres, LArg, LZ, NVM], PV), PN2 = [N, PName, Adres, LArg, LZ, NVM], S = vcall_acc(PN2, A2), n_l(Arg, VAL, PV, A2), n_b_wyr(T, VAL, PV, G)
						;	S = H, n_b_wyr(T, VAL, PV, G).



thunk_checked(NamedADR1, NamedADR) :- thunk_trace_loop(NamedADR1, [], ListaDoPoprawki), thunk_corrected(ListaDoPoprawki, NamedADR1, NamedADR).

thunk_trace_loop(NamedADR1, List2, OstatecznaListaDoPoprawki) :- 
											thunk_trace(NamedADR1, List2, ListaDoPoprawki, Czy),
											(Czy =:= 1, !, thunk_trace_loop(NamedADR1, ListaDoPoprawki, OstatecznaListaDoPoprawki); OstatecznaListaDoPoprawki = ListaDoPoprawki).



thunk_trace(blok(deklarations_obj(Deks), instrukcja_zlozona(IsNSrc)), ListaDoPoprawki, ListaDoPoprawki2, Czy) :- !,
																								thunk_trace_inst(IsNSrc, ListaDoPoprawki, ListaPoInstrukcjach, Czy),
																								one_up2(ListaPoInstrukcjach, LPIU),
																								thunk_trace_deks(Deks, LPIU, ListaDoPoprawki3, Czy),
																								one_down2(ListaDoPoprawki3, ListaDoPoprawki2).


thunk_trace(program(B, _), Lista1, DoPoprawki, Czy) :- thunk_trace(B, Lista1, DoPoprawki, Czy), (var(Czy), !, Czy = 0; true). 

one_up2([], []) :- !.

one_up2([H|T], [G|S]) :- H = [ND, Name, Nr], ND2 is ND + 1, G = [ND2, Name, Nr], one_up2(T, S).


one_down2([], []) :- !.

one_down2([H|T], [G|S]) :- H = [ND, Name, Nr], ND2 is ND - 1, G = [ND2, Name, Nr], one_down2(T, S).



thunk_trace_inst([], L, L, _) :- !.

thunk_trace_inst([H|T], DoPoprawki, PostDoPoprawki, Czy) :- 
													H = assign(_, Wyr), !, thunk_trace_wyr(Wyr, DoPoprawki, Po, Czy), thunk_trace_inst(T, Po, PostDoPoprawki, Czy)
										
												;	H = if(Bool, IT, IE), !, thunk_trace_wyr(Bool, DoPoprawki, Po1, Czy), thunk_trace_inst(IT, Po1, Po2, Czy), thunk_trace_inst(IE, Po2, Po3, Czy), thunk_trace_inst(T, Po3, PostDoPoprawki, Czy)
										
												;	H = if(Bool, IT), !,thunk_trace_wyr(Bool, DoPoprawki, Po1, Czy), thunk_trace_inst(IT, Po1, Po2, Czy), thunk_trace_inst(T, Po2, PostDoPoprawki, Czy)
										
												;	H = while(Bool, Iz), !,thunk_trace_wyr(Bool, DoPoprawki, Po1, Czy), thunk_trace_inst(Iz, Po1, Po2, Czy), thunk_trace_inst(T, Po2, PostDoPoprawki, Czy)
										
												;	H = call_acc(PN, A), !, thunk_trace_call([PN, A], DoPoprawki, Po, Czy), thunk_trace_inst(T, Po, PostDoPoprawki, Czy)
										
												;	H = return(Wyr), !, thunk_trace_wyr(Wyr, DoPoprawki, Po, Czy), thunk_trace_inst(T, Po, PostDoPoprawki, Czy)
										
												;	H = read_acc(_), !, thunk_trace_inst(T, DoPoprawki, PostDoPoprawki, Czy)
										
												;	H = write_acc(Wyr), thunk_trace_wyr(Wyr, DoPoprawki, Po, Czy), thunk_trace_inst(T, Po, PostDoPoprawki, Czy).


thunk_trace_wyr([], L, L, _) :- !.

thunk_trace_wyr([H|T], L1, L2, Czy) :- 
									H = vcall_acc(PN, A), !, thunk_trace_call([PN, A], L1, L3, Czy), thunk_trace_wyr(T, L3, L2, Czy)
								;	thunk_trace_wyr(T, L1, L2, Czy).


thunk_trace_call([[N, PName, _, _, _, NVM], A], List, List2, Czy) :- thunk_trace_args(A, [N, PName, 0, NVM], List, List2, Czy).

thunk_trace_args([], _, L, L, _) :- !.

thunk_trace_args([H|T], [ND, PName, Nr, [G|S]], List, List2, Czy) :- 
									H = [var_obj(_)], !, Nr2 is Nr + 1, thunk_trace_args(T, [ND, PName, Nr2, S], List, List2, Czy)
								;	H = [arg_obj([_, _, _,value, _])], !, Nr2 is Nr + 1, thunk_trace_args(T, [ND, PName, Nr2, S], List, List2, Czy)
								;	H = [arg_obj([N, _, N2, name, BPN])], !, Nr2 is Nr + 1, (NShift is N + 1, G = name, member([NShift, BPN, N2], List), \+member([ND, PName, Nr], List), Czy = 1, thunk_trace_args(T, [ND, PName, Nr2, S], [[ND, PName, Nr]|List], List2, Czy); thunk_trace_args(T, [ND, PName, Nr2, S], List, List2, Czy))
								;	Nr2 is Nr + 1, (G = name, \+member([ND, PName, Nr], List), Czy = 1, ListaUpDate = [[ND, PName, Nr]|List], thunk_trace_wyr(H, ListaUpDate, List3, Czy), thunk_trace_args(T, [ND, PName, Nr2, S], List3, List2, Czy) ;
																thunk_trace_wyr(H, List, List3, Czy), thunk_trace_args(T, [ND, PName, Nr2, S], List3, List2, Czy) ). 



thunk_trace_deks([], L, L, _) :- !.

thunk_trace_deks([H|T], DoPoprawki, PostDoPoprawki, Czy) :- 
												H = procedura(_, _, LBlok, _, _, _, _), !,
												thunk_trace(LBlok, DoPoprawki, Po, Czy),
												thunk_trace_deks(T, Po, PostDoPoprawki, Czy)
											;	thunk_trace_deks(T, DoPoprawki, PostDoPoprawki, Czy).






thunk_corrected(LDP, blok(deklarations_obj(Deks), instrukcja_zlozona(Is)), blok(deklarations_obj(Deks2), instrukcja_zlozona(Is2))) :- !, thunk_corrected_inst(LDP, Is, Is2), one_up2(LDP, LDP2), thunk_corrected_deks(LDP2, Deks, Deks2).



thunk_corrected(ListaDoPoprawki, program(T, L), program(TC, L)) :- thunk_corrected(ListaDoPoprawki, T, TC).



thunk_corrected_inst(_, [], []) :- !.

thunk_corrected_inst(LDP, [H|T], [G|S]) :- 
													H = assign(Z, Wyr), !, thunk_corrected_wyr(LDP, Wyr, Wyr2), thunk_corrected_var(LDP, Z, Z2), G = assign(Z2, Wyr2), thunk_corrected_inst(LDP, T, S)
										
												;	H = if(Bool, IT, IE), !, thunk_corrected_wyr(LDP, Bool, Bool2), thunk_corrected_inst(LDP, IT, IT2), thunk_corrected_inst(LDP, IE, IE2), G = if(Bool2, IT2, IE2), thunk_corrected_inst(LDP, T, S)
										
												;	H = if(Bool, IT), !, thunk_corrected_wyr(LDP, Bool, Bool2), thunk_corrected_inst(LDP, IT, IT2), G = if(Bool2, IT2), thunk_corrected_inst(LDP, T, S)
										
												;	H = while(Bool, Iz), !, thunk_corrected_wyr(LDP, Bool, Bool2), thunk_corrected_inst(LDP, Iz, Iz2), G = while(Bool2, Iz2), thunk_corrected_inst(LDP, T, S)
										
												;	H = call_acc(PN, A), !, thunk_corrected_call(LDP, PN, A, PN2, A2), G = call_acc(PN2, A2), thunk_corrected_inst(LDP, T, S)
										
												;	H = return(Wyr), !, thunk_corrected_wyr(LDP, Wyr, Wyr2), G = return(Wyr2), thunk_corrected_inst(LDP, T, S)
										
												;	H = read_acc(A), !, thunk_corrected_var(LDP, A, A2), G = read_acc(A2), thunk_corrected_inst(LDP, T, S)
										
												;	H = write_acc(Wyr), thunk_corrected_wyr(LDP, Wyr, Wyr2), G = write_acc(Wyr2), thunk_corrected_inst(LDP, T, S).

thunk_corrected_wyr(_, [], []) :- !.

thunk_corrected_wyr(LDP, [H|T], [G|S]) :-
									H = arg_obj([N, A, N2, name, PName]), N3 is N + 1, member([N3, PName, N2], LDP), !, G = arg_obj([N, A, N2, thunk, PName]), thunk_corrected_wyr(LDP, T, S)
								;	H = vcall_acc(PN, A), !, thunk_corrected_call(LDP, PN, A, PN2, A2), G = vcall_acc(PN2, A2), thunk_corrected_wyr(LDP, T, S)
								;	G = H, thunk_corrected_wyr(LDP, T, S).


thunk_corrected_call(LDP, [N, PName, Adres, LArg, LZ, NVM], A, [N, PName, Adres, LArg, LZ, NVM2], A2) :- thunk_corrected_nvm(LDP, N, PName, 0, NVM, NVM2), thunk_corrected_arguments(LDP, A, A2).


thunk_corrected_nvm(_, _, _, _, [], []) :- !.

thunk_corrected_nvm(LDP, N, PName, Nr, [H|T], [G|S]) :- H = name, member([N, PName, Nr], LDP), !, G = thunk, Nr2 is Nr + 1, thunk_corrected_nvm(LDP, N, PName, Nr2, T, S)
													;	G = H, Nr2 is Nr + 1, thunk_corrected_nvm(LDP, N, PName, Nr2, T, S).


thunk_corrected_arguments(_, [], []) :- !.
thunk_corrected_arguments(LDP, [H|T], [G|S]) :- thunk_corrected_wyr(LDP, H, G), thunk_corrected_arguments(LDP, T, S). 


thunk_corrected_var(LDP, A, A2) :- A = arg_obj([N, A, N2, name, PName]), member([N, PName, N2], LDP), !, false
								;	A2 = A.	



thunk_corrected_deks(_, [], []) :- !.

thunk_corrected_deks(LDP, [H|T], [G|S]) :-
								H = procedura(Name, Argumenty, LBlok, Adres, LArg, LZ, NameValueMap), !,
								thunk_corrected(LDP, LBlok, LBlok2),
								thunk_corrected_nvm0(LDP, Name, NameValueMap, NameValueMap2, 0),  /*-1*/	
								thunk_corrected_arguments0(LDP, Name, Argumenty, Argumenty2, 0),							
								G = procedura(Name, Argumenty2, LBlok2, Adres, LArg, LZ, NameValueMap2),
								thunk_corrected_deks(LDP, T, S)
							;	G = H, thunk_corrected_deks(LDP, T, S).


thunk_corrected_nvm0(_, _, [], [], _) :- !.
thunk_corrected_nvm0(LDP, Name, [H|T], [G|S], Nr) :- H = name, member([1, Name, Nr], LDP), !, G = thunk, Nr2 is Nr + 1, thunk_corrected_nvm0(LDP, Name, T, S, Nr2)
												;	G = H, Nr2 is Nr + 1, thunk_corrected_nvm0(LDP, Name, T, S, Nr2).

thunk_corrected_arguments0(_, _, [], [], _) :- !.
thunk_corrected_arguments0(LDP, Name, [H|T], [G|S], Nr) :- H = arg_obj([name,ArgsName]), member([1, Name, Nr], LDP), !, G = arg_obj([thunk, ArgsName]), Nr2 is Nr + 1, thunk_corrected_arguments0(LDP, Name, T, S, Nr2)
												;	G = H, Nr2 is Nr + 1, thunk_corrected_arguments0(LDP, Name, T, S, Nr2).



compiled(NamedADR, HighLevelAssemblerADR) :- extract_procedures(NamedADR, EP), EP = [[_,_,LZP]|_], with_flatened_instructions(EP, FI), with_p_start_and_end(FI, LZP, HighLevelAssemblerADR).



extract_procedures(program(blok(D, instrukcja_zlozona(I)), LZP), [H|T]) :- H=[_, I, LZP], extract_procedures(D, T).

extract_procedures(deklarations_obj([]), []) :- !.

extract_procedures(deklarations_obj([G|S]), T) :- G=vars_obj(_), !, extract_procedures(deklarations_obj(S), T)
												;	G = procedura(Name, _, LBlok, Adres, _, _, _), LBlok = blok(D, instrukcja_zlozona(I)), H = [Adres, I, Name], extract_procedures(D, L), extract_procedures(deklarations_obj(S), L2), append(L, L2, R), T = [H|R].


with_flatened_instructions([], []) :- !.
with_flatened_instructions([H|T], [S|G]) :- H=[A,I|_], f_i(I, IFl), S = bloczek(A, _, IFl), with_flatened_instructions(T, G).

f_i([], []) :- !.
f_i([H|T], [G|S]) :- 
					
					H = assign(var_obj([N, _, N2]), Wyr), !, licz(Wyr, LiczWyr), for(N, [swapa, load], ForNswapaload),
				
					G = [
							LiczWyr,
							
							wez_stala(1), swapd, wez_z(fffd), ForNswapaload,  sub, swapa, load, swapd, swapa,  sub, acc_arithm(2,  sub), acc_arithm(N2,  sub),
							
							swapd, wez_stos, swapd, swapa, swapd, store,
							
							wez_stala(1), swapd, wez_stala(ffff), swapa, load, add, store  
						],
																	
						f_i(T, S)
					
					
				;	H = assign(arg_obj([N, _, N2, name, _]), Wyr), !, licz(Wyr, LiczWyr), for(N, [swapa, load], ForNswapaload),
				
					G = [
							LiczWyr,
							
							wez_stala(3), swapd, wez_z(fffd), ForNswapaload,  sub, acc_arithm(N2,  sub),
							
							swapa, load,
							
							swapd, wez_stos, swapd, swapa, swapd, store,
							
							wez_stala(1), swapd, wez_stala(ffff), swapa, load, add, store 
						],
																	
						f_i(T, S)
				
				
				;	H = assign(arg_obj([N, _, N2, value, _]), Wyr), !, licz(Wyr, LiczWyr), for(N, [swapa, load], ForNswapaload),
				
					G = [
							LiczWyr,
							
							wez_stala(3), swapd, wez_z(fffd), ForNswapaload,  sub, acc_arithm(N2,  sub),
							
							swapd, wez_stos, swapd, swapa, swapd, store,
							
							wez_stala(1), swapd, wez_stala(ffff), swapa, load, add, store 
						],
																	
						f_i(T, S)
				
				
				;	H = if(Bool, ITh, IEl), !, f_i(ITh, FITh), f_i(IEl, FIEl), licz(Bool, LiczBool),
				
				
					G = [
							LiczBool, wez_stos, swapa, wez_stala(IElAdres), swapa, branchz, 
							
							bloczek(_, _, [[wez_stala(1), swapd, wez_stala(ffff), swapa, load, add, store], FITh]),
							
							wez_stala(EAK), jump,
							
							bloczek(IElAdres, EAK, [[wez_stala(1), swapd, wez_stala(ffff), swapa, load, add, store], FIEl])
						],
																																
						f_i(T, S)
				
				
				
				
				;	H = if(Bool, ITh), !, f_i(ITh, TFI), licz(Bool, LiczBool),
					
					G = [
							LiczBool, wez_stos, swapa, wez_stala(TAK), swapa, branchz, 
							
							bloczek(_, TAK, [[wez_stala(1), swapd, wez_stala(ffff), swapa, load, add, store],TFI])
						],
																				
						f_i(T, S)
				
				
				
				
				;	H = while(Bool, Izl), !, f_i(Izl, IZlF), licz(Bool, LiczBool),
					
					G = bloczek(WA, WAK, [
						
						
							LiczBool, wez_stos, swapa, wez_stala(WAK), swapa, branchz, 
							
							bloczek(_, _, [[wez_stala(1), swapd, wez_stala(ffff), swapa, load, add, store], IZlF]), 
							
							wez_stala(WA), jump
						
					]),
					
					f_i(T, S)
				
				;	H = call_acc([N, _, Adres, LArg, LZ, NVM], Args), !, ewaluacja_argumentow(Args, NVM, Ev, LArg), for(N, [swapa, load], ForNswapaload),
				
					G = [
							bloczek(_, KW, [ 
							
												wez_stala(1), swapd, wez_z(ffff),  sub, swapa, wez_stala(0), store,
							
												wez_stala(1), swapd, swapa,  sub, swapa, wez_stala(KW), store,
							
												wez_stala(1), swapd, swapa,  sub, swapd, wez_z(fffe), swapd, swapa, swapd, store,  
							
												wez_stala(1), swapd, swapa,  sub, swapd, wez_z(fffd), ForNswapaload, swapd, swapa, swapd, store,
							
												wez_stala(1), swapd, swapa,  sub, swapa, wez_stala(LArg), store,
							
												wez_stala(1), swapd, swapa,  sub, swapa, wez_stala(LZ), store,
							
												wez_stala(LArg), swapd, swapa,  sub, odloz(ffff),
							
												Ev,
							
												wez_stala(LArg), swapd, wez_z(ffff), add, acc_arithm(2, add),
							
												odloz(fffe), odloz(fffd),
							
												wez_stala(LZ), swapd, wez_z(ffff),  sub, store, 
							
												wez_stala(Adres), jump
							
											]),
											
							wez_stala(1), swapd, wez_z(ffff), add, store
						],
						
						f_i(T, S)
				
				
				
				
				;	H = return(Wyr), !, licz(Wyr, LiczWyr),
				
										 G = [
												LiczWyr, wez_stala(3), swapd, wez_z(fffe), add, swapd, wez_z(ffff), swapa, load, swapd, swapa, swapd, store,
										
												wez_stala(3), swapd, wez_z(fffe), add, odloz(ffff),
												
												wez_stala(1), swapd, wez_z(fffe), add, swapa, load, odloz(fffd),
												
												wez_stala(2), swapd, wez_z(fffe), add, swapa, load, odloz(fffc),
												
												wez_z(fffd), odloz(fffe),
												
												wez_z(fffc), jump
											],
											
											f_i(T, S)
				
				;	H = read_acc(var_obj([N, _, N2])), !, for(N, [swapa, load], ForNswapaload),
				
					G = [ 
							wez_stala(1), swapd, wez_z(fffd), ForNswapaload,  sub, swapa, load, swapd, swapa,  sub, acc_arithm(2,  sub), acc_arithm(N2,  sub),
							
							swapa, wez_stala(1), syscall, store
						],
						
						f_i(T, S)
				
				
				;	H = read_acc(arg_obj([N, _, N2, name, _])), !, for(N, [swapa, load], ForNswapaload),
					
					G = [	
							wez_stala(3), swapd, wez_z(fffd), ForNswapaload,  sub, acc_arithm(N2,  sub),
							
							swapa, load,
							
							swapa, wez_stala(1), syscall, store
						],
						
						f_i(T, S)
				
				
				;	H = read_acc(arg_obj([N, _, N2, value, _])), !, for(N, [swapa, load], ForNswapaload),
					
					G = [	
							wez_stala(3), swapd, wez_z(fffd), ForNswapaload,  sub, acc_arithm(N2,  sub),
							
							swapa, wez_stala(1), syscall, store
						],
						
					f_i(T, S)
				
				
				
				;	H = write_acc(Wyr), licz(Wyr, LiczWyr),
					
					G = [
							LiczWyr, wez_stos, swapd, wez_stala(2), syscall,
							
							wez_stala(1), swapd, wez_stala(ffff), swapa, load, add, store 
						],
						
						f_i(T, S).

ewaluacja_argumentow([], _, [], 0) :- !.

ewaluacja_argumentow([Arg|Args], [ANV|NVM], [AEv|Ev], N) :- ewaluacja_argumentu(Arg, ANV, AEv, N), N2 is N - 1, ewaluacja_argumentow(Args, NVM, Ev, N2).


ewaluacja_argumentu(Arg, ANV, AEv, N) :-
							ANV = value, !, licz(Arg, LiczArg), AEv = 	[
																			LiczArg,
														
																			wez_stala(N), swapd, wez_z(ffff), add, 
														
																			swapd, load, swapa, load, swapd, swapa, swapd, store,
																			
																			wez_stala(1), swapd, wez_stala(ffff), swapa, load, add, store 
																		]
						
					
						;	ANV = name, !, (
													
													
													
													Arg = [var_obj([Na, _, N2])], NP is N - 1, for(Na, [swapa, load], ForNswapaload),
																			
																				
																				AEv  =	[
																							wez_z(fffd), ForNswapaload, acc_arithm(1,  sub), swapa, load, swapd, swapa,  sub, acc_arithm(2,  sub), acc_arithm(N2,  sub), odloz(fffc),
																							
																							wez_stala(NP), swapd, wez_z(ffff), add, swapd, wez_z(fffc), swapd, swapa, swapd, store
																						]
												
												
												
												;	Arg = [arg_obj([Na, _, N2, name, _])], NP is N - 1,  for(Na, [swapa, load], ForNswapaload),
																					
																						
																				AEv = 	[
																							wez_z(fffd), ForNswapaload, acc_arithm(3,  sub), acc_arithm(N2,  sub), swapa, load, odloz(fffc),
																									
																							wez_stala(NP), swapd, wez_z(ffff), add, swapd, wez_z(fffc), swapd, swapa, swapd, store
																						]
																					
																					
																							
												;	Arg = [arg_obj([Na, _, N2, value, _])], NP is N - 1, for(Na, [swapa, load], ForNswapaload),
																				
																				
																				 AEv  = [
																							wez_z(fffd), ForNswapaload, acc_arithm(3,  sub), acc_arithm(N2,  sub), odloz(fffc),
																									
																							wez_stala(NP), swapd, wez_z(ffff), add, swapd, wez_z(fffc), swapd, swapa, swapd, store
																						]
											
											
											)
						
						;	ANV = thunk, licz(Arg, LiczArg), NP is N - 1, AEv = 	[
																		wez_stala(NP), swapd, wez_z(ffff), add, swapa, wez_stala(TS), store,
														
																		wez_stala(TE), jump, 
														
																		bloczek(TS, TE, [	
																							LiczArg, wez_z(ffff), acc_arithm(6, add), swapd, wez_z(ffff), swapa, load, swapd, swapa, swapd, store,
													
																							wez_z(ffff), acc_arithm(4, add), swapa, load, odloz(fffd),
												
																							wez_z(ffff), acc_arithm(5, add), swapa, load, odloz(fffc),
																							
																							wez_z(ffff), acc_arithm(6, add), store,
												
																							wez_z(fffd), odloz(fffe),
												
																							wez_z(fffc), jump
																						])
																	].


with_p_start_and_end(Fif, LZP, [PS|FifE]) :-
						PS   = 	[
						
									wez_stala(4), jump, nop, nop,
									
									koniec,
									
									wez_stala(ffff), swapa, wez_stala(fff6), acc_arithm(LZP,  sub), store,
									
									wez_stala(fffe), swapa, wez_stala(fff8), store,
									
									wez_stala(fffd), swapa, wez_stala(fff8), store,
									
									wez_stala(fffa), swapa, wez_stala(2), store,
									
									wez_stala(fff9), swapa, wez_stala(fff9), store,
									
									wez_stala(fff8), swapa, wez_stala(fff8), store,
									
									wez_stala(fff7), swapa, wez_stala(0), store,
									
									wez_stala(fff6), swapa, wez_stala(LZP), store 
									
					  			],
					  
					  	append(Fif, 
					  				[
					  					koniec
					  				],
					  			
					  			FifE).
	
	
	
	
for(0, _, []) :- !.

for(N, A, [H|T]) :- H = A, N2 is N - 1, for(N2, A, T).

licz([], []) :- !.

licz([H|Wyr], [G|Licz]) :-
								H = num_obj(A), !, G = 	[
															wez_stala(1), swapd, wez_stala(ffff), swapa, load,  sub, swapa, wez_stala(A), store,
															
															wez_stala(ffff), swapa, store
														], licz(Wyr, Licz)
							
							;	H = var_obj([N, _, N2]), !, for(N, [swapa, load], ForNswapaload),
															
															
															G = [
																	wez_z(fffd), ForNswapaload, acc_arithm(1,  sub), swapa, load, swapd, swapa,  sub, acc_arithm(2,  sub), acc_arithm(N2,  sub), swapa, load, odloz(fffc),
																	
																	wez_stala(1), swapd, wez_stala(ffff), swapa, load,  sub, swapd, wez_z(fffc), swapd, swapa, swapd, store,
																	
																	wez_stala(ffff), swapa, store
																], licz(Wyr, Licz)
							
							;	H = arg_obj([N, _, N2, value, _]), !, for(N, [swapa, load], ForNswapaload),
							
															G = [
																	wez_z(fffd), ForNswapaload, acc_arithm(3,  sub), acc_arithm(N2,  sub),
							
																	swapa, load, odloz(fffc),
																	
																	wez_stala(1), swapd, wez_stala(ffff), swapa, load,  sub, swapd, wez_z(fffc), swapd, swapa, swapd, store,
																	
																	wez_stala(ffff), swapa, store
																], licz(Wyr, Licz)
							
							;	H = arg_obj([N, _, N2, name, _]), !, for(N, [swapa, load], ForNswapaload),
							
															G = [
																	wez_z(fffd), ForNswapaload, acc_arithm(3,  sub), acc_arithm(N2,  sub),
							
																	swapa, load,
																	
																	swapa, load, odloz(fffc),
																	
																	wez_stala(1), swapd, wez_stala(ffff), swapa, load,  sub, swapd, wez_z(fffc), swapd, swapa, swapd, store,
																	
																	wez_stala(ffff), swapa, store
																], licz(Wyr, Licz)
							
							; H = arg_obj([N, _, N2, thunk, _]), !, for(N, [swapa, load], ForNswapaload),
							
															G = bloczek(_, AKT, [
																					wez_stala(1), swapd, wez_z(fffd), ForNswapaload, add, swapa, load, odloz(fffc),
																	
																					wez_stala(1), swapd, wez_z(ffff),  sub, swapa, wez_stala(0), store,
							
																					wez_stala(1), swapd, swapa,  sub, swapa, wez_stala(AKT), store,
							
																					wez_stala(1), swapd, swapa,  sub, swapd, wez_z(fffe), swapd, swapa, swapd, store,  
									
																					wez_stala(1), swapd, swapa,  sub, swapd, wez_z(fffc), swapd, swapa, swapd, store,
							
																					wez_stala(1), swapd, swapa,  sub, swapa, wez_stala(0), store,
							
																					wez_stala(1), swapd, swapa,  sub, swapa, wez_stala(0), store,
							
																					wez_stala(ffff), swapa, store,
																					
																					wez_z(fffc), odloz(fffd),
																					
																					wez_z(fffe), swapd, wez_z(fffc), odloz(fffe), swapd, odloz(fffc),
																					
																					wez_z(fffc), ForNswapaload, acc_arithm(3,  sub), acc_arithm(N2,  sub), swapa, load,
																					
																					jump
																				]), licz(Wyr, Licz)
						
						
							; H = vcall_acc([N, _, Adres, LArg, LZ, NVM], Args), !, ewaluacja_argumentow(Args, NVM, Ev, LArg), for(N, [swapa, load], ForNswapaload),
							
														G = bloczek(_, KW, [ 
							
																				wez_stala(1), swapd, wez_z(ffff),  sub, swapa, wez_stala(0), store,
							
																				wez_stala(1), swapd, swapa,  sub, swapa, wez_stala(KW), store,
							
																				wez_stala(1), swapd, swapa,  sub, swapd, wez_z(fffe), swapd, swapa, swapd, store,  
									
																				wez_stala(1), swapd, swapa,  sub, swapd, wez_z(fffd), ForNswapaload, swapd, swapa, swapd, store,
							
																				wez_stala(1), swapd, swapa,  sub, swapa, wez_stala(LArg), store,
							
																				wez_stala(1), swapd, swapa,  sub, swapa, wez_stala(LZ), store,
							
																				wez_stala(LArg), swapd, swapa,  sub, odloz(ffff),
							
																				Ev,
							
																				wez_stala(LArg), swapd, wez_z(ffff), add, acc_arithm(2, add),
							
																				odloz(fffe), odloz(fffd),
							
																				wez_stala(LZ), swapd, wez_z(ffff),  sub, store, 
							
																				wez_stala(Adres), jump
							
																			]), licz(Wyr, Licz)
									
							; H = add_acc, !, G = 	[
														wez_z(ffff), swapa, load, swapa,
								
														swapd, wez_stala(1), swapd, add, swapa, swapd, load,
														
														add, store, wez_stala(ffff), swapa, store 
													], licz(Wyr, Licz)
				
							; H = substract_acc, !, G = [
															wez_z(ffff), swapa, load, swapa,
								
															swapd, wez_stala(1), swapd, add, swapa, swapd, load,
															
															sub, store, wez_stala(ffff), swapa, store
														], licz(Wyr, Licz)
														
							; H = mult_acc, !, G = [
															wez_z(ffff), swapa, load, swapa,
								
															swapd, wez_stala(1), swapd, add, swapa, swapd, load,
															
															mul, store, wez_stala(ffff), swapa, store
														], licz(Wyr, Licz)
														
							; H = div_acc, !, G   =	[
														wez_z(ffff), swapa, load, swapa,
								
														swapd, wez_stala(1), swapd, add, swapa, swapd, load,
															
														div, store, wez_stala(ffff), swapa, store
													], licz(Wyr, Licz)
							
							; H = mod_acc, !, G = 	[
														wez_z(ffff), swapa, load, swapa,
								
														swapd, wez_stala(1), swapd, add, swapa, swapd, load,
															
														div, swapd, wez_stala(fff0), swapd, shift, store, wez_stala(ffff), swapa, store
													], licz(Wyr, Licz)
							
							; H = opp, !, G =   [
													wez_z(ffff), swapa, load, 
													
													swapd, wez_stala(ffff), swapd, mul, store
												], licz(Wyr, Licz)
							
							
							
							; H = less_rel, !, G =  [
														wez_stala(1), swapd, wez_z(ffff), swapd , add, swapa, load,
														
														swapd, wez_stala(1), swapd, shift,
														
														wez_stala(1), swapd, wez_z(ffff), swapd , add, swapa, load,
														
														swapd, wez_z(ffff), swapa, load, swapd,
															
														sub, swapd, wez_stala(ffff), swapd, shift,
														
														swapa, wez_stala(P), swapa, branchn,
														
														bloczek(_, _,   [
																			wez_stala(1), swapd, wez_z(ffff), add, swapa, swapd, store,
																			
																			wez_stala(ffff), swapa, store
																		]),
														wez_stala(K), jump,
														bloczek(P, K, [
																			wez_stala(1), swapd, wez_z(ffff), add, swapa, wez_stala(0), store,
																			
																			wez_stala(ffff), swapa, store
																		])
													], licz(Wyr, Licz)
							
							; H = lesseq_rel, !, G = [
														wez_stala(1), swapd, wez_z(ffff), swapa, load, shift,
														
														wez_z(ffff), swapa, load, swapa,
								
														swapd, wez_stala(1), swapd, add, swapa, swapd, load, swapd,
															
														sub,swapd, wez_stala(ffff), swapd, shift,
														 swapa, wez_stala(P), swapa, branchn,
														
														bloczek(_, _,   [
																			wez_stala(1), swapd, wez_z(ffff), add, swapa, swapd, store,
																			
																			wez_stala(ffff), swapa, store
																		]),
														wez_stala(K), jump,
														bloczek(P, K, [
																			wez_stala(1), swapd, wez_z(ffff), add, swapa, wez_stala(0), store,
																			
																			wez_stala(ffff), swapa, store
																		])
													], licz(Wyr, Licz)
							
							; H = gr_rel, !, G =  [
														wez_stala(1), swapd, wez_z(ffff), swapa, load, shift,
														
														wez_z(ffff), swapa, load, swapa,
								
														swapd, wez_stala(1), swapd, add, swapa, swapd, load, swapd,
															
														sub, swapd, wez_stala(ffff), swapd, shift, 
														swapa, wez_stala(P), swapa, branchn,
														
														bloczek(_, _,   [
																			wez_stala(1), swapd, wez_z(ffff), add, swapa, wez_stala(0), store,
																			
																			wez_stala(ffff), swapa, store
																		]),
														wez_stala(K), jump,
														bloczek(P, K, [
																			wez_stala(1), swapd, wez_z(ffff), add, swapa, swapd, store,
																			
																			wez_stala(ffff), swapa, store
																		])
													], licz(Wyr, Licz)
							
							; H = greq_rel, !, G =  [
														wez_stala(1), swapd, wez_z(ffff), swapd , add, swapa, load,
														
														swapd, wez_stala(1), swapd, shift,
														
														wez_stala(1), swapd, wez_z(ffff), swapd, add, swapa,
														
														load, swapd, wez_z(ffff), swapa, load, swapd,
															
															sub,swapd, wez_stala(ffff), swapd, shift,
															 swapa, wez_stala(P), swapa, branchn,
														
														bloczek(_, _,   [
																			wez_stala(1), swapd, wez_z(ffff), add, swapa, wez_stala(0), store,
																			
																			wez_stala(ffff), swapa, store
																		]),
														wez_stala(K), jump,
														bloczek(P, K, [
																			wez_stala(1), swapd, wez_z(ffff), add, swapa, swapd, store,
																			
																			wez_stala(ffff), swapa, store
																		])
													], licz(Wyr, Licz)
							
							
							; H = eq_rel, !, G = [
													wez_z(ffff), swapa, load, swapa,
								
													swapd, wez_stala(1), swapd, add, swapa, swapd, load,
															
													sub, swapa, wez_stala(P), swapa, branchz,
													
													bloczek(_, _,   [
																			wez_stala(1), swapd, wez_z(ffff), add, swapa, wez_stala(0), store,
																			
																			wez_stala(ffff), swapa, store
																	]),
														wez_stala(K), jump,
													bloczek(P, K, [
																		wez_stala(1), swapd, wez_z(ffff), add, swapa, swapd, store,
																			
																		wez_stala(ffff), swapa, store
																	])
												 ], licz(Wyr, Licz)
							
							; H = noteq_rel, !, G = [
														wez_z(ffff), swapa, load, swapa,
								
														swapd, wez_stala(1), swapd, add, swapa, swapd, load,
															
														sub, swapa, wez_stala(P), swapa, branchz,
													
														bloczek(_, _,   [
																			wez_stala(1), swapd, wez_z(ffff), add, swapa, swapd, store,
																			
																			wez_stala(ffff), swapa, store
																		]),
														wez_stala(K), jump,
														bloczek(P, K, [
																			wez_stala(1), swapd, wez_z(ffff), add, swapa, wez_stala(0), store,
																				
																			wez_stala(ffff), swapa, store
																		])
													], licz(Wyr, Licz)
													
							
							
							
							
							
						
							; H = or_dep, !, G = [
													wez_z(ffff), swapa, load, swapa,
								
													swapd, wez_stala(1), swapd, add, swapa, swapd, load,
														
													add, swapa, wez_stala(P), swapa, branchz,
													
													bloczek(_, _,   [
																			wez_stala(1), swapd, wez_z(ffff), add, swapa, swapd, store,
																			
																			wez_stala(ffff), swapa, store
																	]),
														wez_stala(K), jump,
													bloczek(P, K, [
																		wez_stala(1), swapd, wez_z(ffff), add, swapa, wez_stala(0), store,
																				
																		wez_stala(ffff), swapa, store
																	])
												 ], licz(Wyr, Licz)
													
							; H = and_dep, !, G = [
													wez_z(ffff), swapa, load, swapa,
								
													swapd, wez_stala(1), swapd, add, swapa, swapd, load,
														
													mul, swapa, wez_stala(P), swapa, branchz,
													
													bloczek(_, _,   [
																			wez_stala(1), swapd, wez_z(ffff), add, swapa, swapd, store,
																			
																			wez_stala(ffff), swapa, store
																	]),
														wez_stala(K), jump,
													bloczek(P, K, [
																		wez_stala(1), swapd, wez_z(ffff), add, swapa, wez_stala(0), store,
																				
																		wez_stala(ffff), swapa, store
																	])
												 ], licz(Wyr, Licz)
													
													
							; H = not_dep, G = 	[
														wez_z(ffff), swapa, load, swapa, wez_stala(P), swapa, branchz,
														
														bloczek(_, _,   [
																			wez_stala(1), swapd, wez_z(ffff), add, swapa, wez_stala(0), store,
																			
																			wez_stala(ffff), swapa, store
																	]),
														wez_stala(K), jump,
														bloczek(P, K, [
																			wez_stala(1), swapd, wez_z(ffff), add, swapa, swapd, store,
																			
																			wez_stala(ffff), swapa, store
																		])
													], licz(Wyr, Licz).
									
																
	
low_leveled(HighLevelAssemblerADR, SextiumMachineCodeBlocks) :- flattted(HighLevelAssemblerADR, F1), macros(F1, F2), flattted(F2, F3), hexdecimal_to_decimal(F3, SextiumMachineCodeBlocks). 


flattted(HighLevelAssemblerADR, F1) :- flatten(HighLevelAssemblerADR, F), flatted(F, F1).


flatted([], []) :- !.
flatted([H|T], [S|G]) :- H = bloczek(P, K, I), !, flattted(I, I2), S = bloczek(P, K, I2), flatted(T, G)
						;	S = H, flatted(T, G).



macros([], []) :- !.

macros([H|T], [G|S]) :- H = bloczek(P, K, I), !, macros(I, I2),  G = bloczek(P, K, I2), macros(T, S)
					;	H = wez_z(Adres), !, G = [wez_stala(Adres), swapa, load], macros(T, S)
					;	H = acc_arithm(Value, Op), !, G = [swapd, wez_stala(Value), swapd, Op], macros(T, S)
					;	H = wez_stos, !, G = [wez_stala(ffff), swapa, load, swapa, load], macros(T, S)
					;	H = odloz(Adres), !, G = [swapa, wez_stala(Adres), swapa, store], macros(T, S)
					;	H = koniec, !, G = [wez_stala(0), syscall, nop, nop], macros(T, S)
					;	G = H, macros(T, S).


hexdecimal_to_decimal([], []) :- !.
hexdecimal_to_decimal([H|T], [G|S]) :- H = bloczek(P, K, I), !, hexdecimal_to_decimal(I, I2), G = bloczek(P, K, I2), hexdecimal_to_decimal(T, S)
									;	H = wez_stala(A), h_t_d(A, B), !, G = wez_stala(B), hexdecimal_to_decimal(T, S)
									;	G = H, hexdecimal_to_decimal(T, S).
									
									
h_t_d(A, B) :-
				A == ffff, !, B = 65535
			;	A == fffe, !, B = 65534
			;	A == fffd, !, B = 65533
			;	A == fffc, !, B = 65532
			;	A == fffb, !, B = 65531
			;	A == fffa, !, B = 65530
			;	A == fff9, !, B = 65529
			;	A == fff8, !, B = 65528
			;	A == fff7, !, B = 65527
			;	A == fff6, !, B = 65526
			;	A == fff0, B = 65520.


flated(SextiumMachineCodeBlocks,SextiumMachineCode) :- words(SextiumMachineCodeBlocks, [], Words), constans(Words, Constans), flattted(Constans, ConstansF), adres_count(ConstansF, 0, AC), extract(AC, PF), flatten(PF, SextiumMachineCode).



words([], L, W) :- L = [], !, W = [] ; filled_with_nopes(L, L2), reverse(L2, L3), W = [slowo(L3)].


words([H|T], L, [S|W]) :- H = bloczek(P, K, I), words(I, [], I2), (L = [], !, S = bloczek(P, K, I2), words(T, [], W) ; !, filled_with_nopes(L, L2), reverse(L2, L3), S = slowo(L3), G = bloczek(P, K, I2), words(T, [], R), W = [G|R]  )
						;	 length(L, N), (N < 3, !, words(T, [H|L], [S|W]) ; N =:= 3, reverse([H|L], L2), S = slowo(L2), words(T, [], W) ).


filled_with_nopes([], []) :- !.
filled_with_nopes(L, L2) :- length(L, N), N2 is 4 - N,  add_n_nopes(L, N2, L2).

add_n_nopes(L, 0, L) :- !.
add_n_nopes(L, N, L2) :- N2 is N - 1, add_n_nopes([nop|L], N2, L2).


constans([], []) :- !.
  
constans([H|T], [S|G]) :- H = bloczek(P, K, I), !, constans(I, I2), S = bloczek(P, K, I2), constans(T, G)
					; H = slowo(A), przerob(A, S), constans(T, G).
			
			
przerob(A, S) :- zamien(A, A2), stale(A, As), S = [slowo(A2)|As].

zamien([], []) :- !.
zamien([H|T], [G|S]) :- H = wez_stala(_), !, G = const, zamien(T, S) ; G = H, zamien(T, S).
					
stale([], []) :- !.

stale([H|T], S) :- H = wez_stala(Stala), !, G = slowo(Stala), stale(T, R), S = [G|R] ; stale(T, S).

adres_count([], _, []) :- !.
adres_count([H|T], N, [G|S]) :- H = bloczek(P, K, I), !, P = N, wyznacz(bloczek(N, K, I)), adres_count(I, N, I2), G = bloczek(P, K, I2), adres_count(T, K, S) ; G = H, N3 is N + 1, adres_count(T, N3, S).


wyznacz(bloczek(P, K, I)) :- dlugosc(I, N), K is P + N.

dlugosc([], 0) :- !.
dlugosc([H|T], N) :- H = bloczek(_,_,I), !, dlugosc(I, N2), dlugosc(T, N3), N is N2 + N3 ; dlugosc(T, N4), N is N4 + 1.




extract([], []) :- !.
extract([H|T], [G|S]) :- H = bloczek(_,_,I), !, extract(I, I2), G = I2, extract(T, S) ; G = H, extract(T, S).





binarized([], []) :- !.

binarized([H|T], [G|S]) :- H = slowo([A, B, C, D]), !, digits([A, B, C, D], Di), hexdec_number(Di, G), binarized(T, S) ; H = slowo(G), binarized(T, S).

digits([], []) :- !.

digits([H|T], [G|S]) :- 	(
							H = nop, !, G = 0
						;	H = syscall, !, G = 1
						;	H = load, !, G = 2
						;	H = store, !, G = 3
						;	H = swapa, !, G = 4
						;	H = swapd, !, G = 5
						;	H = branchz, !, G = 6
						;	H = branchn, !, G = 7
						;	H = jump, !, G = 8
						;	H = const, !, G = 9
						;	H = add, !, G = 10
						;	H = sub, !, G = 11
						;	H = mul, !, G = 12
						;	H = div, !, G = 13
						;	H = shift, !, G = 14
						;	H = nand, G = 15 ), digits(T, S).

hexdec_number([A,B,C,D], N) :- N is D + C * 16 + B * 256 + A * 4096.

