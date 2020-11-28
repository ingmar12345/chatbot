%=====================================================================
% Filename:    alice.pl
% Author:      Diego Molla
% Date:        14/08/02
% Version:     1.0
%
% Purpose:
% This file contains a Prolog implementation of the ALICE chatterbot.
% The top predicate is loop/0, and examples of category definitions
% are:
%
% category([
%	   pattern([can,you,star(A),'?']),
%	   template(['I', 'don''t', really, know, if,'I','can', A,
%	 	     but,'I''m', very, good, at, swimming])
%	  ]).
%
% category([
%	   pattern([yes]),
%	   that([do, you, like, movies,'?']),
%	   template(['What', is, your, favourite, movie, '?'])
%	  ]).
%
% category([
%	   pattern([star(_),i,have,no,star(A)]),
%	   template([think(set_var(it,A)),
%		     'I',see,'.','Would',you,like,
%		     to,have,it,'?'])
%	  ]).
%
% category([
%	   pattern([yes]),
%	   that([star(_),'Would',you,like,to,have,it,'?']),
%	   template([think(get_var(it,V)),
%		     'Where',do,you,usually,get,V,'?'])
%	   ]).
%
% category([
%	   pattern([star(A),alice]),
%	   template([srai(A)])
%          ]).
%
% category([
%	   pattern([i,like,star(A),with,syntax(np(B)),star(C)]),
%	   template(['Did',you,say,C,'?','I',also,like,A,with,B,
%		     think(set_var(it,A))])
%         ]).
%
% category([
%	   pattern([star(_)]),
%	   template([random([
%		 	     [tell,me,more,about,think(get_var(it,V)),V],
%			     [do,you,like,dancing,'?']])])
%          ]).
%
%=======================================================================

:- use_module(library(lists),[member/2,nth0/3,append/3]).
:- use_module(library(random),[random/3]).

:- dynamic alice_var/2.

%=======================================================================
% loop.
% loop(+Context).
% loop/0 is the top predicate.
% loop/1 uses the argument  to keep track of the context.

loop:-
	loop([hello]).

loop(Context):-
	interact_once(Context,NewContext),!,   % Try once only
	loop(NewContext).

%
%=======================================================================

%=======================================================================
% interact_once(+Context,-NewContext)
% Do one interaction

interact_once(Context,NewContext):-
	read_atomics(L),
	find_and_reply(L,Context,NewContext).

%
%=======================================================================

%=======================================================================
% find_and_reply(+Input,+Context,-NewContext)
% Find a rule that matches the input and produce the response

find_and_reply(Input,Context,NewContext):-
	% Category with "that"
	category(C),
	member(that(TH),C),
	tokenise(TH,THTokens),
	tokenise(Context,CTokens),
	input_match(THTokens,CTokens),
	member(pattern(P),C),
	tokenise(P,PTokens),
	input_match(PTokens,Input),
	member(template(T),C),
	generate_response(T,Context,NewContext),nl.

find_and_reply(Input,Context,NewContext):-
	% Category without "that"
	category(C),
	\+ member(that(_),C), % Added 3/10/2002
	member(pattern(P),C),
	tokenise(P,PTokens),
	input_match(PTokens,Input),
	member(template(T),C),
	generate_response(T,Context,NewContext),nl.

%=======================================================================
% find_and_reply(+Input,+Context,-NewContext)
% Find a rule that matches the input and produce the response

find_and_reply_srai(Input,Context,NewContext):-
	% Category with "that"
	category(C),
	member(that(TH),C),
	tokenise(TH,THTokens),
	tokenise(Context,CTokens),
	input_match(THTokens,CTokens),
	member(pattern(P),C),
	tokenise(P,PTokens),
	tokenise(Input,ITokens),
	input_match(PTokens,ITokens),
	member(template(T),C),
	generate_response(T,Context,NewContext).

find_and_reply_srai(Input,Context,NewContext):-
	% Category without "that"
	category(C),
	\+ member(that(_),C), % Added 3/10/2002
	member(pattern(P),C),
	tokenise(P,PTokens),
	tokenise(Input,ITokens),
	input_match(PTokens,ITokens),
	member(template(T),C),
	generate_response(T,Context,NewContext).

%
%========================================================================

%========================================================================
% input_match(+Pattern,+Input)
% Succeed if the input line matches the pattern. All variables in the
% pattern result instantiated in the process.

input_match([],[]).

input_match([H|T],[H|T2]):-
	input_match(T,T2).

input_match([star([])|T],Input_Line):-
	input_match(T,Input_Line).

input_match([star([H|TStar])|T],[H|T2]):-
	input_match([star(TStar)|T],T2).

input_match([syntax(SynCat,Match)|T],Input_Line):-
	PredCall =.. [SynCat,Input_Line,Rest],
	call(PredCall),                 % Call to the grammar rules
	append(Match,Rest,Input_Line),
	input_match(T,Rest).

input_match([syntax(SynCat,Match,Features)|T],Input_Line):-
	append([SynCat|Features],[Input_Line,Rest],PredCallList),
	PredCall =.. PredCallList,
	call(PredCall),                 % Call to the grammar rules
	append(Match,Rest,Input_Line),
	input_match(T,Rest).

%
%=======================================================================

%=======================================================================
% generate_response(+Template,+Context,-Response)
% Output the response that corresponds to the template and the given
% context. The output argument Response contains the generated
% response tokenised so that it can be used as the context of the
% next interaction.

generate_response([],_,[]).

generate_response([think(Commands)|T],Context,FinalResponse):-
	!,
	% "think" element
	call(Commands),
	generate_response(T,Context,FinalResponse).

generate_response([srai(SRAIList)|T],Context,FinalResponse):-
	!,
	% "srai" element
	flatten_list(SRAIList,FlattenedList),
	find_and_reply_srai(FlattenedList,Context,SRAIResponse),
	generate_response(T,Context,TResponse),
	append(SRAIResponse,TResponse,FinalResponse).

generate_response([random(RandomList)|T],Context,FinalResponse):-
	!,
	% "random" element
	list_length(RandomList,Length),
	random(0,Length,Random),
	nth0(Random,RandomList,ChosenList),
	generate_response(ChosenList,Context,ListResponse),
	generate_response(T,Context,TResponse),
	append(ListResponse,TResponse,FinalResponse).

generate_response([H|T],Context,FinalResponse):-
	H = [_|_],
	!,
	% A list; we need to flatten out all lists
	generate_response(H,Context,HResponse),
	generate_response(T,Context,TResponse),
	append(HResponse,TResponse,FinalResponse).

generate_response([H|T],Context,[H|TResponse]):-
	% Default rule
	write(H),
	write(' '),
	generate_response(T,Context,TResponse).

%
%======================================================================

%======================================================================
% flatten_list(+List,-Flattened)
% Utility predicate that flattens out a list

flatten_list([],[]).
flatten_list([[]|T],Flat):-
	flatten_list(T,Flat).
flatten_list([H|T],Flat):-
	H = [_|_],
	flatten_list(H,FlattenedHead),
	flatten_list(T,FlattenedTail),
	append(FlattenedHead,FlattenedTail,Flat).
flatten_list([H|T],[H|FlatTail]):-
	\+ H = [],
	\+ H = [_|_],
	flatten_list(T,FlatTail).

%
%======================================================================

%======================================================================
% list_length(+List,-N)
% Utility predicate that returns the length of the list

list_length([],0).

list_length([_|T],L):-
	list_length(T,L2),
	L is L2 + 1.

%
%======================================================================

%======================================================================
% get_var(+VarName,?Value)
% set_var(+VarName,+Value)
% Utility predicates to get the value of a Bot variable or set the
% variable

get_var(VarName,Value):-
	alice_var(VarName,Value).

get_var(VarName,[]):-
	\+ alice_var(VarName,_).

set_var(VarName,Value):-
	retractall(alice_var(VarName,_)),
	tokenise(Value,Tokens),
	asserta(alice_var(VarName,Tokens)).

%
%=======================================================================

%=======================================================================
% tokenise(+Atom,-List)
% Convert an atom representing text into a list of tokens. This
% predicate makes heavy use of the definitions in readatom.pl (below)

tokenise([],[]):-
	!.
tokenise([star(A)|T],[star(A)|Tokenised]):-
	!,
	tokenise(T,Tokenised).
tokenise([syntax(A,B)|T],[syntax(A,B)|Tokenised]):-
	!,
	tokenise(T,Tokenised).
tokenise([Atom|T],Tokens):-
	atomic(Atom),
	!,
	tokenise_atom(Atom,AtomTokens),
	tokenise(T,TTokens),
	append(AtomTokens,TTokens,Tokens).
tokenise(Input,Input):-
	write('WARNING: Unable to tokenise '),
	writeq(Input).

tokenise_atom(Atom,List):-
	name(Atom,String),
	tokenise_string(String,List).

% tokenise_string(String,Atomics)
%  Counterpart of read_atomics/1 below

tokenise_string([],Atomics):-
	complete_string([],nil,end,Atomics).

tokenise_string([C|Tail],Atomics):-
	char_type(C,Type,Char),
	complete_string(Tail,Char,Type,Atomics).

% complete_string(+Buffer,+FirstC,+FirstT,-Atomics)
%   Counterpart of complete_line/3 below

complete_string(_,_,end,[]) :- !.                  % stop at end

complete_string(B,_,blank,Atomics) :-              % skip blanks
   !,
   tokenise_string(B,Atomics).

complete_string(B,FirstC,special,[A|Atomics]) :-   % special char
   !,
   name(A,[FirstC]),
   tokenise_string(B,Atomics).

complete_string(B,FirstC,alpha,[A|Atomics]) :-     % begin word
   complete_string_word(B,BOut,FirstC,alpha,Word,NextC,NextT),
   name(A,Word),  % may not handle numbers correctly - see text
   complete_string(BOut,NextC,NextT,Atomics).

% complete_string_word(+BufferIn,-BufferOut,+FirstC,+FirstT,
%                      -List,-FollC,-FollT)
% counterpart of complete_word/5 below

complete_string_word([],[],FirstC,alpha,[FirstC|List],FollC,FollT) :-
   !,
   complete_string_word([],[],nil,end,List,FollC,FollT).

complete_string_word([C|BTail],BOut,FirstC,alpha,[FirstC|List],FollC,FollT) :-
   !,
   char_type(C,NextT,NextC),
   complete_string_word(BTail,BOut,NextC,NextT,List,FollC,FollT).

complete_string_word(B,B,FirstC,FirstT,[],FirstC,FirstT).
   % where FirstT is not alpha

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% File READATOM.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Appendix B

% Version of read_atomics/1 for most Prologs.  See text.


% read_atomics(-Atomics)
%  Reads a line of text, breaking it into a
%  list of atomic terms: [this,is,an,example].

read_atomics(Atomics) :-
   read_char(FirstC,FirstT),
   complete_line(FirstC,FirstT,Atomics).


% read_char(-Char,-Type)
%  Reads a character and runs it through char_type/3.

read_char(Char,Type) :-
   get0(C),
   char_type(C,Type,Char).


% complete_line(+FirstC,+FirstT,-Atomics)
%  Given FirstC (the first character) and FirstT (its type), reads
%  and tokenizes the rest of the line into atoms and numbers.

complete_line(_,end,[]) :- !.                  % stop at end

complete_line(_,blank,Atomics) :-              % skip blanks
   !,
   read_atomics(Atomics).

complete_line(FirstC,special,[A|Atomics]) :-   % special char
   !,
   name(A,[FirstC]),
   read_atomics(Atomics).

complete_line(FirstC,alpha,[A|Atomics]) :-     % begin word
   complete_word(FirstC,alpha,Word,NextC,NextT),
   name(A,Word),  % may not handle numbers correctly - see text
   complete_line(NextC,NextT,Atomics).


% complete_word(+FirstC,+FirstT,-List,-FollC,-FollT)
%  Given FirstC (the first character) and FirstT (its type),
%  reads the rest of a word, putting its characters into List.

complete_word(FirstC,alpha,[FirstC|List],FollC,FollT) :-
   !,
   read_char(NextC,NextT),
   complete_word(NextC,NextT,List,FollC,FollT).

complete_word(FirstC,FirstT,[],FirstC,FirstT).
   % where FirstT is not alpha


% char_type(+Code,?Type,-NewCode)
%  Given an ASCII code, classifies the character as
%  'end' (of line/file), 'blank', 'alpha'(numeric), or 'special',
%  and changes it to a potentially different character (NewCode).

char_type(10,end,10) :- !.         % UNIX end of line mark
char_type(13,end,13) :- !.         % DOS end of line mark
char_type(-1,end,-1) :- !.         % get0 end of file code

char_type(Code,blank,32) :-        % blanks, other ctrl codes
  Code =< 32,
  !.

char_type(Code,alpha,Code) :-      % digits
  48 =< Code, Code =< 57,
  !.

char_type(Code,alpha,Code) :-      % lower-case letters

  97 =< Code, Code =< 122,
  !.

char_type(Code,alpha,NewCode) :-   % upper-case letters
  65 =< Code, Code =< 90,
  !,
  NewCode is Code + 32.            %  (translate to lower case)

char_type(Code,special,Code).      % all others

%--End-------------------------------------------------------------------
