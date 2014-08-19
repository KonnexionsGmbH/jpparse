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
-export([parsetree/1]).

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
