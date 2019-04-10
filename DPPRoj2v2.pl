:- ensure_loaded(library(clpfd)).


puzzle_solution([[_R1,R11,R12],[R2,Diagnal,R22],[R3,R31,Diagnal]]):-
	allInRange([[Diagnal,R22],[R31,Diagnal]]),
	valid([[_,R11,R12],[R2,Diagnal,R22],[R3,R31,Diagnal]]).

puzzle_solution([[_R1,R11,R12,R13],[R2,Diagnal,R22,R23],[R3,R31,Diagnal,R33],[R4,R41,R42,Diagnal]]):-
	allInRange([[Diagnal,R22,R23],[R31,Diagnal,R33],[R41,R42,Diagnal]]),
	valid([[_,R11,R12,R13],[R2,Diagnal,R22,R23],[R3,R31,Diagnal,R33],[R4,R41,R42,Diagnal]]).


allInRange([]).
allInRange([X|XS]):-
		validNum(X),
		allInRange(XS),
		checkDifferent([X|XS]),
		transpose([X|XS],Col),
		checkDifferent(Col).

validNum([]).
validNum([X|XS]):-
	between(1,9,X),
	validNum(XS).

checkDifferent([]).
checkDifferent([X|XS]):-
	all_different(X),
	checkDifferent(XS).


valid(Puzzle):-
	validMath(Puzzle),
	transpose(Puzzle,Col),
	validMath(Col).

validMath([_|XS]):-
	validArth(XS).

validArth([]).
validArth([X|XS]):-
	isValid(X),
	validArth(XS).

isValid([X|XS]):-
	checkSum(X,XS,0,_Sum);
	checkMul(X,XS,1,_Mul).

checkSum(Aim, [], Sum, Sum) :- Aim = Sum.
checkSum(Aim, [X|XS], Acc, Sum) :-
	Accu is Acc + X,
	checkSum(Aim, XS, Accu, Sum).

checkMul(Aim, [], Mul, Mul) :- Aim = Mul.
checkMul(Aim, [X|XS], Acc, M) :-
	Accu is Acc * X,
	checkMul(Aim, XS, Accu, M).
