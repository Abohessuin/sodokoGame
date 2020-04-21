%first state or inpute
intialstate([[1,1,0,first],[1,2,2,first],[1,3,6,first],[1,4,0,second],[1,5,0,second],[1,6,0,second],[1,7,8,third],[1,8,1,third],[1,9,0,third],[2,1,3,first],[2,2,0,first],[2,3,0,first],[2,4,7,second],[2,5,0,second],[2,6,8,second],[2,7,0,third],[2,8,0,third],[2,9,6,third],[3,1,4,first],[3,2,0,first],[3,3,0,first],[3,4,0,second],[3,5,5,second],[3,6,0,second],[3,7,0,third],[3,8,0,third],[3,9,7,third],[4,1,0,fourth],[4,2,5,fourth],[4,3,0,fourth],[4,4,1,fifth],[4,5,0,fifth],[4,6,7,fifth],[4,7,0,sixth],[4,8,9,sixth],[4,9,0,sixth],[5,1,0,fourth],[5,2,0,fourth],[5,3,3,fourth],[5,4,9,fifth],[5,5,0,fifth],[5,6,5,fifth],[5,7,1,sixth],[5,8,0,sixth],[5,9,0,sixth],[6,1,0,fourth],[6,2,4,fourth],[6,3,0,fourth],[6,4,3,fifth],[6,5,0,fifth],[6,6,2,fifth],[6,7,0,sixth],[6,8,5,sixth],[6,9,0,sixth],[7,1,1,seventh],[7,2,0,seventh],[7,3,0,seventh],[7,4,0,eigth],[7,5,3,eigth],[7,6,0,eigth],[7,7,0,ninth],[7,8,0,ninth],[7,9,2,ninth],[8,1,5,seventh],[8,2,0,seventh],[8,3,0,seventh],[8,4,2,eigth],[8,5,0,eigth],[8,6,4,eigth],[8,7,0,ninth],[8,8,0,ninth],[8,9,9,ninth],[9,1,0,seventh],[9,2,3,seventh],[9,3,8,seventh],[9,4,0,eigth],[9,5,0,eigth],[9,6,0,eigth],[9,7,4,ninth],[9,8,6,ninth],[9,9,0,ninth]]).

%our goal state
goal([[1,1,7,first],[1,2,2,first],[1,3,6,first],[1,4,4,second],[1,5,9,second],[1,6,3,second],[1,7,8,third],[1,8,1,third],[1,9,5,third],[2,1,3,first],[2,2,1,first],[2,3,5,first],[2,4,7,second],[2,5,2,second],[2,6,8,second],[2,7,9,third],[2,8,4,third],[2,9,6,third],[3,1,4,first],[3,2,8,first],[3,3,9,first],[3,4,6,second],[3,5,5,second],[3,6,1,second],[3,7,2,third],[3,8,3,third],[3,9,7,third],[4,1,8,fourth],[4,2,5,fourth],[4,3,2,fourth],[4,4,1,fifth],[4,5,4,fifth],[4,6,7,fifth],[4,7,6,sixth],[4,8,9,sixth],[4,9,3,sixth],[5,1,6,fourth],[5,2,7,fourth],[5,3,3,fourth],[5,4,9,fifth],[5,5,8,fifth],[5,6,5,fifth],[5,7,1,sixth],[5,8,2,sixth],[5,9,4,sixth],[6,1,9,fourth],[6,2,4,fourth],[6,3,1,fourth],[6,4,3,fifth],[6,5,6,fifth],[6,6,2,fifth],[6,7,7,sixth],[6,8,5,sixth],[6,9,2,sixth],[7,1,1,seventh],[7,2,9,seventh],[7,3,4,seventh],[7,4,8,eigth],[7,5,3,eigth],[7,6,6,eigth],[7,7,5,ninth],[7,8,7,ninth],[7,9,2,ninth],[8,1,5,seventh],[8,2,6,seventh],[8,3,7,seventh],[8,4,2,eigth],[8,5,1,eigth],[8,6,4,eigth],[8,7,3,ninth],[8,8,8,ninth],[8,9,9,ninth],[9,1,2,seventh],[9,2,3,seventh],[9,3,8,seventh],[9,4,5,eigth],[9,5,7,eigth],[9,6,9,eigth],[9,7,4,ninth],[9,8,6,ninth],[9,9,1,ninth]]).

%get empy cell in current state
%
getemptycell([],_,_,_).
getemptycell([H|T],R,C,S):-
    [RR,CC,NUM,SS]=H,
     (NUM is 0->R is RR,C is CC,S = SS;
     getemptycell(T,R,C,S)).


% in one cell try to put from 1-9 and check in checkvalidnumber rules
% of sodoko.

putnumber([],_,_,[],_).

putnumber([[RR,CC,NUM,_]|T],RE,CE,[Xs|NList],NNum):-
     (RR is RE,CC is CE->
      Xs=[RR,CC,1,_],
      NNum is 1,
      putnumber(T,RE,CE,NList,NNum);
      Xs=[RR,CC,NUM,_],
      putnumber(T,RE,CE,NList,NNum)).

putnumber([[RR,CC,NUM,_]|T],RE,CE,[Xs|NList],NNum):-
     (RR is RE,CC is CE->
      Xs=[RR,CC,2,_],
      NNum is 2,
      putnumber(T,RE,CE,NList,NNum);
      Xs=[RR,CC,NUM,_],
      putnumber(T,RE,CE,NList,NNum)).

putnumber([[RR,CC,NUM,_]|T],RE,CE,[Xs|NList],NNum):-
     (RR is RE,CC is CE->
      Xs=[RR,CC,3,_],
       NNum is 3,
      putnumber(T,RE,CE,NList,NNum);
      Xs=[RR,CC,NUM,_],
      putnumber(T,RE,CE,NList,NNum)).

putnumber([[RR,CC,NUM,_]|T],RE,CE,[Xs|NList],NNum):-
     (RR is RE,CC is CE->
      Xs=[RR,CC,4,_],
      NNum is 4,
      putnumber(T,RE,CE,NList,NNum);
      Xs=[RR,CC,NUM,_],
      putnumber(T,RE,CE,NList,NNum)).

putnumber([[RR,CC,NUM,_]|T],RE,CE,[Xs|NList],NNum):-
     (RR is RE,CC is CE->
      Xs=[RR,CC,5,_],
      NNum is 5,
      putnumber(T,RE,CE,NList,NNum);
      Xs=[RR,CC,NUM,_],
      putnumber(T,RE,CE,NList,NNum)).

putnumber([[RR,CC,NUM,_]|T],RE,CE,[Xs|NList],NNum):-
     (RR is RE,CC is CE->
      Xs=[RR,CC,6,_],
      NNum is 6,
      putnumber(T,RE,CE,NList,NNum);
      Xs=[RR,CC,NUM,_],
      putnumber(T,RE,CE,NList,NNum)).

putnumber([[RR,CC,NUM,_]|T],RE,CE,[Xs|NList],NNum):-
     (RR is RE,CC is CE->
      Xs=[RR,CC,7,_],
      NNum is 7,
      putnumber(T,RE,CE,NList,NNum);
      Xs=[RR,CC,NUM,_],
      putnumber(T,RE,CE,NList,NNum)).

putnumber([[RR,CC,NUM,_]|T],RE,CE,[Xs|NList],NNum):-
     (RR is RE,CC is CE->
      Xs=[RR,CC,8,_],
      NNum is 8,
      putnumber(T,RE,CE,NList,NNum);
      Xs=[RR,CC,NUM,_],
      putnumber(T,RE,CE,NList,NNum)).


putnumber([[RR,CC,NUM,_]|T],RE,CE,[Xs|NList],NNum):-
     (RR is RE,CC is CE->
      Xs=[RR,CC,9,_],
      NNum is 9,
      putnumber(T,RE,CE,NList,NNum);
      Xs=[RR,CC,NUM,_],
      putnumber(T,RE,CE,NList,NNum)).


%check sodoko rules
%assume any check of 3 function is 1 mean unique


%check about row there is only one unique numbers in it
rowuniqueness([],_,_,_,Res,Res).
rowuniqueness([[RR,CC,NUM,_]|T],R,C,NNum,Checkrow,Res):-
  (R is RR,C is CC-> rowuniqueness(T,R,C,NNum,Checkrow,Res);
    (NUM is NNum ,R is RR->Checkrow is 0;
     rowuniqueness(T,R,C,NNum,Checkrow,Res))).




%check about Square there is only one unique numbers in it
columnuniqueness([],_,_,_,Res,Res).
columnuniqueness([[RR,CC,NUM,_]|T],R,C,NNum,Checkrow,Res):-
  (R is RR,C is CC->
    columnuniqueness(T,R,C,NNum,Checkrow,Res);
    (NUM is NNum,C is CC->Checkrow is 0;
     columnuniqueness(T,R,C,NNum,Checkrow,Res))).




%check about column there is only one unique numbers in it
squareuniqueness([],_,_,_,_,Res,Res).
squareuniqueness([[RR,CC,NUM,SS]|T],R,C,S,NNum,Checkrow,Res):-
  (R is RR,C is CC->
    squareuniqueness(T,R,C,S,NNum,Checkrow,Res);
    (NUM is NNum,SS =S ->Checkrow is 0;
     squareuniqueness(T,R,C,S,NNum,Checkrow,Res))).





% current state is true if satisfy sodokorules so use this function to
% check this
sodokorules(Currentstate,R,C,S,NNum):-
  Check1=1,Check2=1,Check3=1,
 rowuniqueness(Currentstate,R,C,NNum,Check1,R1),
 columnuniqueness(Currentstate,R,C,NNum,Check2,R2),
 squareuniqueness(Currentstate,R,C,S,NNum,Check3,R3),
 R1\=0,R2\=0,R3\=0.




%fill new cell and check if it valid and if it true return
% new state with filled cell
fillblankcell(Currentstate,R,C,S,Newstate):-
    putnumber(Currentstate,R,C,Newstate,NNum),
    sodokorules(Newstate,R,C,S,NNum).




%get empty cell in the list of lists and fil it with all valid numbers
%return new state
createnewstate(Currentstate,Stackofnodes,VisitedNodes,Newstate,R,C,S):-
     fillblankcell(Currentstate,R,C,S,Newstate),
    \+member(Newstate,VisitedNodes),
    \+member(Newstate,Stackofnodes).




%get state from statck to expand and check if goal or not
getcurrentnode([Currentnode|Restofstack],Currentnode,Restofstack).



%creaTE NEW STATES OF current nodes
getchildernofcurrnode(Currentnode, Stackofnodes ,VisitedNodes,Children,R,C,S):-
		bagof(Newstate, createnewstate( Currentnode, Stackofnodes, VisitedNodes, Newstate,R,C,S),
                      Children).
getchildernofcurrnode(_,_,_,[],_,_,_).


%put new childern in the last of stack
addListToOpen(Children,[],Children).
addListToOpen(L, [H|Open], [H|NewOpen]):-
		addListToOpen(L, Open, NewOpen).



%bfs to reach to goal through shortest path of states

bfsSearch([],_,_):-
		write('No solution'),nl,!.

bfsSearch([Goal|_], VisitedNodes, Goal):-
		write('A solution is found'), nl ,
		printsolution(Goal,VisitedNodes),!.

bfsSearch(Stackofnodes,VisitdetNodes,Goal):-
    getcurrentnode(Stackofnodes,Currentnode,Restofstack),
    getemptycell(Currentnode,R,C,S),
    getchildernofcurrnode(Currentnode,Stackofnodes,VisitdetNodes,Children,R,C,S),
    addListToOpen(Children , Restofstack, NewStackofnodes),
    bfsSearch(NewStackofnodes,[Currentnode|VisitdetNodes],Goal).


startGame():-
    intialstate(NOT),
    goal(LOT),
    bfsSearch([NOT],[],LOT).






























