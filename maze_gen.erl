-module(maze_gen).
-export([maze/2]).
-author("Nicolas Favre-Felix - n.favrefelix#gmail.com").
-license("Public Domain").

-import(lists,[seq/2, nth/2, delete/2, dropwhile/2, member/2, merge/2, map/2]).

maze(W,H) -> 
	Walls = shuffle(vert(W,H) ++ horiz(W,H)),
	Cells = cells(W,H),
	do_maze(Cells, Walls, []).

horiz(W,H) -> [{X,Y,h} || X <- seq(0,W-1), Y <- seq(1,H-1)].
vert(W,H) -> [{X,Y,v} || X <- seq(1,W-1), Y <- seq(0,H-1)].
cells(W,H) -> [[{X,Y}] || X <- seq(0,W-1), Y <- seq(0,H-1)].

shuffle(L) -> shuffle(length(L), L).
shuffle(1, L) -> L;
shuffle(N, L) ->	% Knuth's shuffle
	K = random:uniform(N-1),
	{At_K, At_N} = {nth(K, L), nth(N, L)},
	ReplaceFun = fun(X) -> case X of
				At_K -> At_N;
				At_N -> At_K;
				Other -> Other 
			end
		end,
	shuffle(N-1, map(ReplaceFun, L)). % tail-recursive!

neighbours({X,Y,v}) ->  {{X-1, Y}, {X, Y}};
neighbours({X,Y,h}) ->  {{X, Y-1}, {X, Y}}.

do_maze(_, [], Keep) -> Keep;

do_maze(Cells, [W | Walls], Keep) -> 
	{C1, C2} = neighbours(W),
	HasNot = fun(C) -> fun(X) -> not member(C, X) end end,
	Group1 = nth(1, dropwhile(HasNot(C1), Cells)),
	Group2 = nth(1, dropwhile(HasNot(C2), Cells)),
	case Group1 =:= Group2 of  
		false ->  % delete wall, join groups 1 & 2
			WO1 = delete(Group1, Cells),
			WO1_2 = delete(Group2, WO1),
			Joined = merge(Group1, Group2),
			do_maze([Joined|WO1_2], Walls, Keep);
		true ->
			do_maze(Cells, Walls, [W|Keep])
	end.

