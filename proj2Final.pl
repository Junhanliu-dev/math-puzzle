% Name: Junhan Liu
%	ID: 300278821

%This programe is a puzzle solving game that using a uncomplete puzzle as a
%input, and the result would be a complete puzzle shows as a lists of a list,
%each list represents as one row. The role of the puzzle is that, firstly,
%the number of Diagnal line from left top to bottom right must be the same,
% and the numbers of each column and each row must have distinct valiue from
% 1 to 9 and the first number of each row is the sum or multiply with
%the rest of elements in the list, and also the first number of
%each column should be the sum of multiply with the rest of the numbers in that
%column.

%The basic strategy using in this puzzle programe is pattern matching for each
%row and each column to check whether each row or column is fllowinh the role
%of the programe, The programe checks each row to try to make the first row
%follows the role, and check and combine each following row, and change row with
%column to check the role again, and finally, the answer will come up.

:- ensure_loaded(library(clpfd)).

%The pattern matching for each 2x2,3x3 and 4x4 puzzle, the Diagnal is set in the
%predicate.
%the while puzzle can be seen like this:
%					[R1,R11,R12],
%					[R2,Diagnal,R22],
%					[R3,R31,Diagnal].
%After checking whether the rows follow the rule, then it will transpose row
%with column shown as this:
%					[R1,R2,R3],
%					[R11,Diagnal,R31],
%					[R12,R22,Diagnal].
%Because the first line we have alrealy checked as row, and the first row can be
%ignored,and we just only need to check the second line and third line with the
%same rule.
%The first to row is to checkthe whether each row follows the role, if the first
%row satisfy the role and combine it with the second row. If first row satisfies
%the role,then it will cut the potential candidates for searching the right
%answer for the remains of the procedure.
%the 3x3 and 4x4 also follows this puzzle pattern to check each row and
%transpoes it and check the role again.
% The isValid predicate includes three constrain predicates, the first checks
% the range between 1 to 9, and the clpfd in-build predicate all_distinct
% constrains each row has distinct number, and third predicate constrains the
% sum or multiply of the elements (excludes the first element each row)
% is the first element of the row.
puzzle_solution([[_R1,R11,R12],
								[R2,Diagnal,R22],
								[R3,R31,Diagnal]]):-

	isValid([R2,Diagnal,R22]),
	isValid([R3,R31,Diagnal]),
	isValid([R11,Diagnal,R31]),
	isValid([R12,R22,Diagnal]).

%The 3x3 puzzle based on pattern matching, and it works the same as 2x2. the
%first three isValid/1 predicates are to check the rows of the puzzle, and the
%remaining theee predicates are the rows that transposed with column,
%so the original puzzle is like
%								[R1,R11,R12,R13],
%							  [R2,Diagnal,R22,R23],
%								[R3,R31,Diagnal,R33].
%								[R4,R41,R42,Diagnal].
% The puzzle after transposed with column is like :
%								[R1,R2,R3,R4],
%								[R11,Diagnal,R31,R41],
%								[R12,R22,Diagnal,R42],
%								[R13,R23,R33,Diagnal].

puzzle_solution([[_R1,R11,R12,R13],
								[R2,Diagnal,R22,R23],
								[R3,R31,Diagnal,R33],
								[R4,R41,R42,Diagnal]]):-

	isValid([R2,Diagnal,R22,R23]),
	isValid([R3,R31,Diagnal,R33]),
	isValid([R4,R41,R42,Diagnal]),
	isValid([R11,Diagnal,R31,R41]),
	isValid([R12,R22,Diagnal,R42]),
	isValid([R13,R23,R33,Diagnal]).

%The 4x4 puzzle based on pattern matching, and it works the same as 3x3 and 2x2.
% The puzzle before transpose
%						[R1,R11,R12,R13,R14],
%						[R2,Diagnal,R22,R23,R24],
%						[R3,R31,Diagnal,R33,R34],
%						[R4,R41,R42,Diagnal,R44],
%						[R5,R51,R52,R53,Diagnal].
% The puzzle after transpose
%						[R1,R2,R3,R4,R5],
%						[R11,Diagnal,R31,R41,R51],
%						[R12,R22,Diagnal,R42,R52],
%						[R13,R23,R33,Diagnal,R53],
%						[R14,R24,R34,R44,Diagnal].
puzzle_solution([[_R1,R11,R12,R13,R14],
								[R2,Diagnal,R22,R23,R24],
								[R3,R31,Diagnal,R33,R34],
								[R4,R41,R42,Diagnal,R44],
								[R5,R51,R52,R53,Diagnal]]):-

	isValid([R2,Diagnal,R22,R23,R24]),
	isValid([R3,R31,Diagnal,R33,R34]),
	isValid([R4,R41,R42,Diagnal,R44]),
	isValid([R5,R51,R52,R53,Diagnal]),
	isValid([R11,Diagnal,R31,R41,R51]),
	isValid([R12,R22,Diagnal,R42,R52]),
	isValid([R13,R23,R33,Diagnal,R53]),
	isValid([R14,R24,R34,R44,Diagnal]).

%The main predicate that checks whether each row satisfies the sule of the game,
%the predicate devides into two parts, X holds the first element of the row,
%because the first elements is the number that is the sum or multiply of
%remaining elements in the row, so it do not need to check the range and
%distinct, we only need to parse this element to validSumOrMul/2 predicate
%for checking whether the X is the number that is the sum or multiply of
%remaining elements in a row.
%The all_distinct predicate/1 is a clpfd built-in predicate used for checking
% whether the elements of a row contains all distinct value, which is a one
%of game rules that the project specified.
isValid([X|XS]) :-
	checkRange(XS),
	all_distinct(XS),
	validSumOrMul(X,XS).

% the checkRange predicate/1 for checking whether each element of a row in the
% range from 1 to 9, if X satisfies the range, then it will recursively check
% XS untill the row is empty, if any one of the elements is out of rante, it will
% directly return the false. The member predicate/1 is the function to check
% is the element in the range of 1 to 9.
checkRange([]).
checkRange([X|XS]):-
	Range = [1,2,3,4,5,6,7,8,9],
	member(X,Range),
	checkRange(XS).

% The validSumOrMul predicate/2 is used for checking whether the first element of
% a row is the sum or product of the remaining elements of the row, which is
% also works with transposed puzzle because they are the same as it just transposed
% the row with the column.
% The X is the first element of a row, and rest of the list will be XS, if the
% valisSum does not satisfy the X, it will try the multiply, if both are false
% it will return false.
validSumOrMul(X,XS):-
	valisSum(X,XS,0,_S);
	validMul(X,XS,1,_M).

% The predicate/4, the Aim is the first element in a row, and [X|XS] is the rest
% elements in a row, Acc is the accumulator to contain the sum of all remaining
% elements in a row, and it return true iff the Ain is equal to Sum.
valisSum(Aim, [], Sum, Sum) :- Aim = Sum.
valisSum(Aim, [X|XS], Acc, Sum) :-
	Accu is Acc + X,
	valisSum(Aim, XS, Accu, Sum).
% The predicate/4 worls the similar as the valisSum, which is to check the product
% of remaining elements is equal to the first element in a row, it will return
% true iff the Aim is equal to Mul, the Acc is also a accumulator to store the
% the result.
validMul(Aim, [], Mul, Mul) :- Aim = Mul.
validMul(Aim, [X|XS], Acc, M) :-
	Accu is Acc * X,
	validMul(Aim, XS, Accu, M).
