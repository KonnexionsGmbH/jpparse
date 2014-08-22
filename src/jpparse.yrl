%% -*- erlang -*-
Header "%% Copyright (C) K2 Informatics GmbH"
"%% @private"
"%% @Author Bikram Chatterjee"
"%% @Email chatterjee@bluewin.ch".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Nonterminals
 jp
 args
 leaf
.

Terminals
 STRING
 ':'
 '['
 '{'
 ','
 ']'
 '}'
 '$'
 '('
 ')'
.

Rootsymbol jp.

Left 500 ':'.
Left 300 '['.
Left 300 '{'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

jp -> '$empty'          : 'empty'.
jp -> leaf              : '$1'.
jp -> jp ':' jp         : {':', flat('$3'), flat('$1')}.
jp -> jp '[' args ']'   : {'[]', '$1', flat('$3')}.
jp -> jp '{' args '}'   : {'{}', '$1', flat('$3')}.
jp -> leaf '(' args ')' : {'fun', '$1', flat('$3')}.

args -> jp              : ['$1'].
args -> jp ',' args     : ['$1' | '$3'].

leaf -> STRING          : unwrap('$1').
leaf -> '$' STRING '$'  : {'$', unwrap('$2')}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Erlang code.

-behaviour(application).
-behaviour(supervisor).

% application callbacks
-export([start/2, stop/1]).

% Supervisor callbacks
-export([init/1]).

% parser and compiler interface
-export([parsetree/1, foldtd/3, foldbu/3, string/1]).

%%--------------------------------------------------------------------------
%%                          dummy application interface
%%--------------------------------------------------------------------------

start(_Type, _Args) -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
stop(_State)        -> ok.
init([])            -> {ok, { {one_for_one, 5, 10}, []} }.

%%--------------------------------------------------------------------------
%%                          parser helper functions
%%--------------------------------------------------------------------------

unwrap({_,_,X}) when is_list(X) ->
    case catch list_to_integer(X) of
        {'EXIT', _} -> list_to_binary(X);
        Integer -> Integer
    end.

flat(['empty']) -> [];
flat(Other) -> Other.

%%--------------------------------------------------------------------------


%%--------------------------------------------------------------------------
%%                                  PARSER
%%--------------------------------------------------------------------------
-spec parsetree(binary()|list()) -> {parse_error, term()} | {lex_error, term()} | {ok, {[tuple()], list()}}.
parsetree(JPath) when is_binary(JPath) -> parsetree(binary_to_list(JPath));
parsetree([]) -> {parse_error, {not_a_valid_json_path, []}};
parsetree(JPath) when is_list(JPath) ->
    catch application:start(jpparse),
    case jsonpath_lex:string(JPath) of
        {ok, Toks, _} ->
            case jpparse:parse(Toks) of
                {ok, PTree} -> {ok, {PTree, Toks}};
                {error, {Line, Module, Message}} ->
                    {parse_error, {Line, lists:flatten(Module:format_error(Message)), Toks}}
            end;
        LexErrorInfo -> {lex_error, jsonpath_lex:format_error(LexErrorInfo)}
    end;
parsetree(SomethingElse) -> {parse_error, {not_a_valid_json_path, SomethingElse}}.

%%--------------------------------------------------------------------------


%%--------------------------------------------------------------------------
%%                                  COMPILER
%%--------------------------------------------------------------------------
-define(TD(__L,__Pt,__AccIn),
        if T =:= td -> Fun(__L,__Pt,__AccIn);
           true -> __AccIn end).
-define(BU(__L,__Pt,__AccIn),
        if T =:= bu -> Fun(__L,__Pt,__AccIn);
           true -> __AccIn end).

string(Pt) ->
    {ok, list_to_binary(foldbu(fun stringfun/3, [], Pt))}.

stringfun(_Depth, {'$',_} = _Pt, Stk) ->
    [A|Rest] = Stk,
    [string:join(["$",A,"$"], "") | Rest];
stringfun(_Depth, {'fun',_,Args} = _Pt, Stk) ->
    {PArgs, [A|Rest]} = lists:split(length(Args), Stk),
    [string:join([A, "("
                  , string:join(lists:reverse(PArgs), ",")
                  , ")"]
                 , "") | Rest];
stringfun(_Depth, {Op,_,Args} = _Pt, Stk)
  when Op =:= '[]'; Op =:= '{}' ->
    {PArgs, [A|Rest]} = lists:split(length(Args), Stk),
    {Lb,Rb} = case Op of
                  '[]' -> {"[","]"};
                  '{}' -> {"{","}"}
              end,
    [string:join([A, Lb
                  , string:join(lists:reverse(PArgs), ",")
                  , Rb]
                 , "") | Rest];
stringfun(_Depth, {':',_,_} = _Pt, Stk) ->
    [B,A|Rest] = Stk,
    [string:join([A,":",B], "")|Rest];
stringfun(_Depth, Pt, Stk) when is_binary(Pt) ->
    [binary_to_list(Pt)|Stk];
stringfun(_Depth, Pt, Stk) when is_integer(Pt) ->
    [integer_to_list(Pt)|Stk].

-spec foldtd(Function :: fun((Depth :: integer()
                            , SubParseTree :: any()
                            , AccIn :: any()) -> AccOut :: any())
             , Acc :: any()
             , ParseTree :: tuple()) -> any().
foldtd(Fun, Acc, Pt) when is_function(Fun, 3) ->
    fold_i({td,Fun}, Acc, Pt).

-spec foldbu(Function :: fun((Depth :: integer()
                            , SubParseTree :: any()
                            , AccIn :: any()) -> AccOut :: any())
             , Acc :: any()
             , ParseTree :: tuple()) -> any().
foldbu(Fun, Acc, Pt) when is_function(Fun, 3) ->
    fold_i({bu,Fun}, Acc, Pt).

% internal fold function
fold_i(Fun, Acc, Pt) ->
    case catch fold_i(Fun, Acc, Pt, 0) of
        {'EXIT',Error} -> {error, Error};
        Result -> Result
    end.
fold_i({T,Fun}, Acc, B, Lvl)
  when is_binary(B); is_integer(B) ->
    Acc1 = ?TD(Lvl,B,Acc),
    ?BU(Lvl,B,Acc1);
fold_i({T,Fun}, Acc, {':', R, L}, Lvl) ->
    Acc1 = ?TD(Lvl,{':', R, L},Acc),
    Acc2 = fold_i({T,Fun}, Acc1, L, Lvl+1),
    Acc3 = fold_i({T,Fun}, Acc2, R, Lvl+1),
    ?BU(Lvl,{':', R, L},Acc3);
fold_i({T,Fun}, Acc, {Op, L, R}, Lvl)
  when ((Op =:=  '{}') orelse (Op =:=  '[]'))
       andalso is_list(R) ->
    Acc1 = ?TD(Lvl,{Op, L, R},Acc),
    Acc2 = fold_i({T,Fun}, Acc1, L, Lvl+1),
    Acc3 = lists:foldl(              
        fun(Ri, Acci) ->
            fold_i({T,Fun}, Acci, Ri, Lvl+1)
        end
        , Acc2, R),
    ?BU(Lvl,{Op, L, R},Acc3);
fold_i({T,Fun}, Acc, {'fun',Fn,Args}, Lvl)
  when is_binary(Fn), is_list(Args) ->
    Acc1 = ?TD(Lvl,{'fun', Fn, Args},Acc),
    Acc2 = fold_i({T,Fun}, Acc1, Fn, Lvl+1),
    Acc3 = lists:foldl(              
        fun(Arg, Acci) ->
            fold_i({T,Fun}, Acci, Arg, Lvl+1)
        end
        , Acc2, Args),
    ?BU(Lvl,{'fun',Fn,Args},Acc3);
fold_i({T,Fun}, Acc, {'$',Tok}, Lvl)
  when is_binary(Tok) ->
    Acc1 = ?TD(Lvl,{'$',Tok},Acc),
    Acc2 = fold_i({T,Fun}, Acc1, Tok, Lvl+1),
    ?BU(Lvl,{'$',Tok},Acc2);
fold_i({T,Fun}, Acc, empty, Lvl) ->
    Acc1 = ?TD(Lvl,<<>>,Acc),
    ?BU(Lvl,<<>>,Acc1);
fold_i(_, _Acc, Pt, _) ->
    exit({unsupported, Pt}).

%%--------------------------------------------------------------------------


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------------
%%                               EUnit test
%%--------------------------------------------------------------------------

parse_test() ->
    ?debugMsg("==========================================="),
    ?debugMsg("|    J S O N   P A T H   P A R S I N G    |"),
    ?debugMsg("==========================================="),
    catch application:start(?MODULE),
    Cwd = filename:absname(""),
    {ShowParseTree, Tests} =
        case file:consult(filename:join([Cwd, "..", "test", "test.txt"])) of
            {ok, [show_parse_tree, T]}  -> {true, T};
            {ok, [_, T]}                -> {false, T};
            {ok, [T]}                   -> {false, T};
            {error, Error}              -> ?assertEqual(ok, Error)
        end,
    ?debugFmt("Test result ~p parse tree"
              , [if ShowParseTree -> with; true -> without end]),
    test_parse(1, ShowParseTree, Tests).

test_parse(_, _, []) -> ok;
test_parse(N, ShowParseTree, [{Test,Target}|Tests]) when is_binary(Test) ->
    test_parse(N, ShowParseTree, [{binary_to_list(Test),Target}|Tests]);
test_parse(N, ShowParseTree, [{Test,Target}|Tests]) ->
    ?debugFmt("[~p]----------------------------------------",[N]),
    ?debugFmt("~ts", [Test]),
    {Tokens,EndLine} = case t_tokenize(Test) of
        {ok,T,E} -> {T,E};
        {error, Error} ->
            ?debugFmt("Tokenize Error ~p", [Error]),
            ?assertEqual(ok, tokenize_error)
    end,
    PTree = case t_parse(Tokens) of
        {ok, PT} -> PT;
        {error, {Line, PError}} ->
            ?debugFmt("Parse Error at ~p : ~s", [Line, PError]),
            ?debugFmt("Tokens ~p:~p", [EndLine,Tokens]),
            ?assertEqual(ok, parsing_error)
    end,
    ?assertEqual(Target, PTree),
    if ShowParseTree -> ?debugFmt("~p", [PTree]); true -> ok end,
    FoldTest = case jpparse:string(PTree) of
        {ok, Ft} -> Ft;
        {error, FError} ->
            ?debugFmt("Folding Error : ~p", [FError]),
            ?debugFmt("ParseTree :~p", [PTree]),
            ?assertEqual(ok, fold_error)
    end,
    ?assertEqual(Test, binary_to_list(FoldTest)),
    test_parse(N+1, ShowParseTree, Tests).

t_tokenize(Test) ->
    case jsonpath_lex:string(Test) of
        {ok,Tokens,EndLine} -> {ok,Tokens,EndLine};
        ErrorInfo -> {error, jsonpath_lex:format_error(ErrorInfo)}
    end.

t_parse(Tokens) ->
    case jpparse:parse(Tokens) of
        {ok, PTree} -> {ok, PTree};
        {error, {Line, Module, Message}} ->
            {error, {Line, lists:flatten(Module:format_error(Message))}}
    end.

%%--------------------------------------------------------------------------

-endif.
