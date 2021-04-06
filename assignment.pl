
:- if(current_predicate(use_rendering/1)).
:- use_rendering(tiles).
:- endif.
:- use_module(library(ordsets)).



% Merge 2 lists
merge_list([], L, L).
merge_list([H|T],L,[H|M]) :-
    merge_list(T,L,M).

%Min/Max predicates
max(A, B, C):-
	A >= B,
	C is A.
max(A, B, C):-
	B > A,
	C is B.

min(A, B, C):-
	A >= B,
	C is B.
min(A, B, C):-
	B > A,
	C is A.

convert_num([X, Y], _, W, L):-
	L is Y*W + X.


% Get list of coordinates and converts to int [0,0] -> 0, [1, 1] -> 10
convert_list_num([], _, _, []).
convert_list_num([L1|L2], H, W, [R1|R2]):-
	convert_num(L1, H, W, R1),
	convert_list_num(L2, H, W, R2).



%Append adjacent infected cells [4,4] -> [3,3], [4,3], [5,3], ...
append_covid([], _, _, _, _).
append_covid([[X, Y]|Cov2], S, Out, H, W) :-
	H1 is H - 1,
	W1 is W - 1,
	V1_x is X - 1,
	V1_y is Y,
	max(0, V1_x, T1_x),
	max(0, V1_y, T1_y),
	V2_x is X - 1,
	V2_y is Y - 1,
	max(0, V2_x, T2_x),
	max(0, V2_y, T2_y),
	V3_x is X - 1,
	V3_y is Y + 1,
	max(0, V3_x, T3_x),
	min(H1, V3_y, T3_y),
	V4_x is X,
	V4_y is Y + 1,
	max(0, V4_x, T4_x),
	min(H1, V4_y, T4_y),
	V5_x is X,
	V5_y is Y - 1,
	max(0, V5_x, T5_x),
	max(0, V5_y, T5_y),
	V6_x is X + 1,
	V6_y is Y + 1,
	min(W1, V6_x, T6_x),
	min(H1, V6_y, T6_y),
	V7_x is X + 1,
	V7_y is Y,
	min(W1, V7_x, T7_x),
	min(H1, V7_y, T7_y),
	V8_x is X + 1,
	V8_y is Y - 1,
	min(W1, V8_x, T8_x), 
	max(0, V8_y, T8_y),
	merge_list(S, [[T1_x, T1_y], [T2_x, T2_y], [T3_x, T3_y], [T4_x, T4_y], [X, Y], [T5_x, T5_y], [T6_x, T6_y], [T7_x, T7_y], [T8_x, T8_y]], L1),
	append_covid(Cov2, L1, L2, H, W),
	merge_list(L1, L2, Out).




%Main function for map generate
map_generate(Covid_quant, Mask_quant, Doctor_quant, H, W, Cov, Mask, Doc, Home) :-
	covid_generate(Covid_quant, H, W, Cov_1),
	mask_generate(Mask_quant, H, W, Cov_1, Mask_1),
	doctor_generate(Doctor_quant, H, W, Cov_1, Mask_1, Doc_1),
	home_generate(H, W, Cov_1, Mask_1, Doc_1, Home_1),
	convert_list(Cov_1, H, W, Cov),
	convert_list(Mask_1, H, W, Mask),
	convert_list(Doc_1, H, W, Doc),
	convert_to_cords(Home_1, H, W, Home),
	write('Covid\n'),
	write(Cov),
	write('\nMask\n'),
	write(Mask),
	write('\nDoc\n'),
	write(Doc),
	write('\nHome\n'),
	write(Home),
	write('\n').


%Covid random generate
covid_generate(C, H, W, L) :-
	E is H*W - 1,
	W_1 is W + 1,
	range(0, E, R), % Get range [0, 1, ... , E]
	ord_subtract(R, [0, 1, W, W_1], R2), % Substract sets
	rnd_select(R2, C, L1),
	convert_list(L1, H, W, L2),
	append_covid(L2,[], Out, H, W),
	convert_list_num(Out, H, W, L4),
	sort(L4, L).

%Random Mask generate under condition not spawn on covid
mask_generate(M, H, W, Cov, L) :-
	E is H*W - 1,
	range(0, E, R),
	ord_subtract(R, Cov, S),
	rnd_select(S, M, L).

% Random Doc generate under conditions
doctor_generate(D, H, W, Cov, Mask, L) :-
	E is H*W - 1,
	range(0, E, R),
	ord_subtract(R, Cov, S1),
	ord_subtract(S1, Mask, S2),
	rnd_select(S2, D, L).

% Random Home generate under conditions
home_generate(H, W, Cov, Mask, Doc, L) :-
	E is H*W - 1,
	range(1, E, R),
	ord_subtract(R, Cov, S1),
	ord_subtract(S1, Mask, S2),
	ord_subtract(S2, Doc, S3),
	rnd_select(S3, 1, L).

% Return list of [S, S+1,..., E-1, E]
range(I,I,[I]).
range(I,K,[I|L]) :- I < K, I1 is I + 1, range(I1,K,L).

%Select N distinct values from list
rnd_select(_,0,[]).
rnd_select(Xs,N,[X|Zs]) :- N > 0,
    length(Xs,L),
    I is random(L) + 1,
    remove_at(X,Xs,I,Ys),
    N1 is N - 1,
    rnd_select(Ys,N1,Zs).

%Remove element in list
remove_at(X,[X|Xs],1,Xs).
remove_at(X,[Y|Xs],K,[Y|Ys]) :- K > 1, 
   K1 is K - 1, remove_at(X,Xs,K1,Ys).

%Select N random variables from range
rand_select(N, S, M, L) :-
	range(S, M, R), rnd_select(R, N, L).	

%Convert int to coords 50 -> [5, 5], 0 -> [0,0]
convert_to_cords(N, _, W, [X,Y]) :-
	X is N mod W,
	Y is N div W.
convert_list([], _, _, []).
convert_list([L1|L2], H, W, [R1|R2]):-
	convert_list(L2, H, W, R2), convert_to_cords(L1, H, W, R1).

/*------------------------------------------------------------------------------------------------
Backtraking section
--------------------------------------------------------------------------------------------------*/



% I == 0 - immune to covid, == 1 - not immune


% Check if current position is valid( not on covid(If no have mask or doctor) or out of bounce or on path)
is_valid([X, Y], _, H, W, 0, Path) :-
	X >= 0,
	X < W,
	Y >= 0,
	Y < H,
	\+ member([X, Y], Path).
is_valid([X, Y], Cov, H, W, 1, Path) :-
	is_valid([X, Y], _, H, W, 0, Path),
	\+ member([X, Y], Cov).

% Length between 2 points
map_length([X, Y], [X1, Y1], L) :-
	D1 is abs(X1 - X),
	D2 is abs(Y1 - Y),
	max(D1, D2, L).

% Return list of adjacent cells in pair with length to home
adj_cells([X, Y], Home, L) :-
	X1 is X + 1,
	Y1 is Y + 1,
	map_length([X1, Y1], Home, H1),
	X2 is X + 1,
	Y2 is Y - 1,
	map_length([X2, Y2], Home, H2),
	X3 is X + 1,
	Y3 is Y,
	map_length([X3, Y3], Home, H3),
	X4 is X,
	Y4 is Y - 1,
	map_length([X4, Y4], Home, H4),
	X5 is X,
	Y5 is Y + 1,
	map_length([X5, Y5], Home, H5),
	X6 is X - 1,
	Y6 is Y + 1,
	map_length([X6, Y6], Home, H6),
	X7 is X - 1,
	Y7 is Y,
	map_length([X7, Y7], Home, H7),
	X8 is X - 1,
	Y8 is Y - 1, 
	map_length([X8, Y8], Home, H8),
	L = [H1-[X1, Y1], H2-[X2, Y2], H3-[X3, Y3], H4-[X4, Y4], H5-[X5, Y5], H6-[X6, Y6], H7-[X7, Y7], H8-[X8, Y8]].

% Augment 1 element to list
aug_list(X, Y, [R1|R2]) :-
	R1 = X,
	R2 = Y.

% Heuristic, return sorted array. On 1st place is best cell to move and so on 
best_path(Pos, Home, L) :-
	adj_cells(Pos, Home, Adj),
	keysort(Adj, L1),
	pairs_values(L1, L).


% Go through all adjacent cells in order, given by best_path
go_through([], _, _, _, _, _, _, _, _, _, _) :-
	garbage_collect(). % Collect garbage to prevent overflow
go_through([R1|R2], C_path, C_len, Pos, I, H, W, Cov, Mask, Doc, Home):-
	(
	is_valid(R1, Cov, H, W, I, C_path) ->
		aug_list(R1, C_path, New_path),
		New_len is C_len + 1,
		back(New_path, New_len, R1, I, H, W, Cov, Mask, Doc, Home),
		go_through(R2, C_path, C_len, Pos, I, H, W, Cov, Mask, Doc, Home)
		;
		go_through(R2, C_path, C_len, Pos, I, H, W, Cov, Mask, Doc, Home)
	).
	



% If on mask or doctor tile without immunioty, move forward without fear
back(C_path, C_len, Pos, 1, H, W, Cov, Mask, Doc, Home) :-
    (member(Pos, Mask);member(Pos, Doc)), 
	back(C_path, C_len, Pos, 0, H, W, Cov, Mask, Doc, Home),!.

% Check if current position is valid, if not, cut.
back(_, _, Pos, I, H, W, Cov, _, _, _) :-
	\+is_valid(Pos, Cov, H, W, I, []),!.

% If current position is home, cut and set new current path to new path
% New path always shorter that old, becouse we cut all paths longer
back(C_path, C_len, Pos, _, _, _, _, _, _, Home) :-
	Pos = Home,
	retractall(min_len(_)),
	assert(min_len(C_len)),
	retractall(s_path(_)),
	assert(s_path(C_path)),!.
	
% If minimal possible length is more than minimal found, cut.
back(_, C_len, Pos, _, _, _, _, _, _, Home) :-
	map_length(Pos, Home, Len),
	Total_len is Len + C_len,
	min_len(S_len),
	Total_len > S_len,!.

% Main backtrack. Get best_path and go through it
back(C_path, C_len, Pos, I, H, W, Cov, Mask, Doc, Home) :-
	best_path(Pos, Home, R),
	go_through(R, C_path, C_len, Pos, I, H, W, Cov, Mask, Doc, Home).

% Start backtracking with given Cov, Mask, Home
start(Path, H, W, Cov, Mask, Doc, Home):-
	E is H*W-1,
	write('\n'),
	H1 is H - 1,
	W1 is W - 1,
	retractall(s_path(_)),
	assert(s_path([[0, 0]])),
	retractall(min_len(_)),
	assert(min_len(E)),
	back([[0, 0]], 0, [0, 0], 1, H, W, Cov, Mask, Doc, Home),
	s_path(Path),
	retractall(s_path(_)),
	retractall(min_len(_)),
	(
		Path = [[0, 0]] ->
		write('fail\n'),
		draw_map(H1, W1, Cov, Mask, Doc, Home, Path)
		;
		draw_map(H1, W1, Cov, Mask, Doc, Home, Path)
	).

% Start backtracking with generated map
start(Path, H, W) :-
	map_generate(2, 1, 1, H, W, Cov, Mask, Doc, Home),
	E is H*W-1,
	write('\n'),
	H1 is H - 1,
	W1 is W - 1,
	retractall(s_path(_)),
	assert(s_path([[0, 0]])),
	retractall(min_len(_)),
	assert(min_len(E)),
	back([[0, 0]], 0, [0, 0], 1, H, W, Cov, Mask, Doc, Home),
	s_path(Path),
	(
	Path = [[0, 0]] ->
	write('fail\n'),
	draw_map(H1, W1, Cov, Mask, Doc, Home, Path)
	;
	draw_map(H1, W1, Cov, Mask, Doc, Home, Path)
	).

% Start A* with generated map
start_a_star(Path, H, W) :-
	map_generate(2, 1, 1, H, W, Cov, Mask, Doc, Home),
	E is H*W-1,
	write('\n'),
	H1 is H - 1,
	W1 is W - 1,
	retractall(s_path(_)),
	assert(s_path([[0, 0]])),
	retractall(min_len(_)),
	assert(min_len(E)),
	a_star([E-1-[[0,0]]-[0,0]], H, W, Cov, Mask, Doc, Home, Path, []),
	(
	Path = [[0, 0]] ->
	write('fail\n'),
	draw_map(H1, W1, Cov, Mask, Doc, Home, Path)
	;
	draw_map(H1, W1, Cov, Mask, Doc, Home, Path)
	).

% Start A* with given map
start_a_star(Path, H, W, Cov, Mask, Doc, Home) :-
	E is H*W-1,
	write('\n'),
	H1 is H - 1,
	W1 is W - 1,
	retractall(s_path(_)),
	assert(s_path([[0, 0]])),
	retractall(min_len(_)),
	assert(min_len(E)),
	a_star([E-1-[0,0]-[0,0]], H, W, Cov, Mask, Doc, Home, Path, []),
	(
	Path = [[0, 0]] ->
	write('fail\n'),
	draw_map(H1, W1, Cov, Mask, Doc, Home, Path)
	;
	draw_map(H1, W1, Cov, Mask, Doc, Home, Path)
	).

% Start both A* and backtracking with same generated map
main_start(Path1, Path2, H, W):-
	map_generate(2, 1, 1, H, W, Cov, Mask, Doc, Home),
	time(start_a_star(Path1, H, W, Cov, Mask, Doc, Home)),
	time(start(Path2, H, W, Cov, Mask, Doc, Home)).

% Draw row
draw_row(_, -1, _, _, _, _, _).
draw_row(H, W, Cov, Mask, Doc, Home, Path):-
	W1 is W - 1,
	draw_row(H, W1, Cov, Mask, Doc, Home, Path),
	print_point(H, W, Cov, Mask, Doc, Home, Path).

% Draw whole map
draw_map(-1, _, _, _, _, _, _).
draw_map(H, W, Cov, Mask, Doc, Home, Path):-
	H1 is H - 1,
	draw_map(H1, W, Cov, Mask, Doc, Home, Path),
	draw_row(H, W, Cov, Mask, Doc, Home, Path),
	write('\n').

% Print point
print_point(W, H, Cov, Mask, Doc, Home, Path) :-
	(member([W, H], Cov),
	write('C'));
	(member([W, H], Mask),
	write('M'));
	(member([W, H], Doc),
	write('D'));
	([W, H] = Home,
	write('H'));
	(member([W, H], Path),
	write('P'));
	write('.').

% Convert list with pairs to list with values
to_list([Value|[]], Out):-
	Out = Value.
to_list([],Out):-
	Out = [].
to_list([Close1|Close2], [Out1|Out2]):-
	pairs_values([Close1], [Value|_]),
	Out1 = Value,
	to_list(Close2, Out2).

% If [X, Y] is not valid, return []
app([X, Y], PopOpen,H, W, Close, Cov, _, _, _, Out) :-
	pairs_keys([PopOpen], T1),
	pairs_keys(T1, T2),
	pairs_values(T2, [I|_]),
	to_list(Close, L),
	\+ is_valid([X, Y], Cov, H, W, I, L),
	Out = [],!.

% If [X, Y] is valid, return pair Heuristic - Immune - Ancestor - Predecessor
app([X, Y], PopOpen, H, W, Close, Cov, Mask, Doc, Home, Out) :-
	pairs_values([PopOpen], Pos),
	pairs_keys([PopOpen], Key1),
	pairs_keys(Key1, Key2),
	pairs_values(Key2, [I|_]),
	to_list(Close, L),
	is_valid([X, Y], Cov, H, W, I, L),
	map_length([X, Y], Home, Heuristic),
	(
	(member([X, Y], Mask);member([X, Y], Doc)) ->
		Out = [Heuristic-0-Pos-[X, Y]]
		;
		Out = [Heuristic-I-Pos-[X, Y]]
	),!.

% Get [X, Y] and finds H-I-[A, B]-[X, Y]
find_list(_, [], _).
find_list(Pos, [List1|_], Out):-
	pairs_values([List1], [Pos|_]),
	Out = List1,!.
find_list(Pos, [List1|List2], Out):-
	find_list(Pos, List2, Out).

% Get H-I-[A, B]-[X, Y] and returns Path from [X, Y] to [0, 0]
get_path(Out1, _, Path):-
	Out1 = [0,0]-[0,0],
	Path = [].
get_path(Out1, List, [Path1|Path2]) :-
	pairs_keys([Out1], [Key|_]),
	pairs_values([Out1], [Path1|_]),
	find_list(Key, List, T3),
	get_path(T3, List, Path2).

% If Open is empty, returns empty
a_star([], _, _, _, _, _, _, Path, _):-
	Path = [[0, 0]].
% If current position is home, return path
a_star(Open, H, W, Cov, Mask, Doc, Home, Path, Close) :-
	keysort(Open, [Out1|Tail]),
	pairs_keys([Out1], V1),
	pairs_keys(V1, V2),
	pairs_keys(V2, [Heu|_]),
	Heu = 0,
	pairs_values([Out1], [P2|_]),
	pairs_values(V1, [P1|_]),
	P1 = [P3|_],
	get_path(P3-P2, Close, Path),!.
% Main a_start predicate get least heuristic and go through it
a_star(Open, H, W, Cov, Mask, Doc, Home, Path, Close) :-
	keysort(Open, [Out1|Tail]),
	a_star_append(Out1, Tail, H, W, Cov, Mask, Doc, Home, Close, Path).

% Go through best heuristic
a_star_append(PopOpen, Tail, H, W, Cov, Mask, Doc, Home, Close, Path) :-
	pairs_values([PopOpen], [[X, Y]|_]),
	pairs_keys([PopOpen], V1),
	pairs_values(V1, [P|_]),
	X1 is X + 1,
	Y1 is Y + 1,
	app([X1, Y1], PopOpen, H, W, Close, Cov, Mask, Doc, Home, Out1),
	X2 is X + 1,
	Y2 is Y - 1,
	app([X2, Y2], PopOpen, H, W, Close, Cov, Mask, Doc, Home, Out2),
	X3 is X + 1,
	Y3 is Y,
	app([X3, Y3], PopOpen, H, W, Close, Cov, Mask, Doc, Home, Out3),
	X4 is X,
	Y4 is Y - 1,
	app([X4, Y4], PopOpen, H, W, Close, Cov, Mask, Doc, Home, Out4),
	X5 is X,
	Y5 is Y + 1,
	app([X5, Y5], PopOpen, H, W, Close, Cov, Mask, Doc, Home, Out5),
	X6 is X - 1,
	Y6 is Y + 1,
	app([X6, Y6], PopOpen, H, W, Close, Cov, Mask, Doc, Home, Out6),
	X7 is X - 1,
	Y7 is Y,
	app([X7, Y7], PopOpen, H, W, Close, Cov, Mask, Doc, Home, Out7),
	X8 is X - 1,
	Y8 is Y - 1, 
	app([X8, Y8], PopOpen, H, W, Close, Cov, Mask, Doc, Home, Out8),
	merge_list(Out1, Out2, T1),
	merge_list(T1, Out3, T2),
	merge_list(T2, Out4, T3),
	merge_list(T3, Out5, T4),
	merge_list(T4, Out6, T5),
	merge_list(T5, Out7, T6),
	merge_list(T6, Out8, T7),
	merge_list(T7, Tail, NewOpen),
	P = [P1|_],
	merge_list([P1-[X, Y]], Close, NewClose),
	a_star(NewOpen, H, W, Cov, Mask, Doc, Home, Path, NewClose).
