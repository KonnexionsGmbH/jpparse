Header "%% Copyright (C) K2 Informatics GmbH"
"%% @private"
"%% @Author Bikram Chatterjee"
"%% @Email chatterjee@bluewin.ch".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Nonterminals
 jsonpath
 jsonpathlist
 jelement
 array
 object
 oabody
.

Terminals
 STRING
 ':'
 '['
 '{'
 ','
 ']'
 '}'
 '-'
.

Rootsymbol jsonpath.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


jsonpath -> jsonpathlist                    : {':', '$1'}.

jsonpathlist -> jelement                    : ['$1'].
jsonpathlist -> jelement ':' jsonpathlist   : ['$1' | '$3'].

jelement -> STRING                          : unwrap('$1').
jelement -> array                           : '$1'.
jelement -> object                          : '$1'.

array -> '[' oabody ']'                     : {'[]', '_', '$2'}.
array -> STRING '[' ']'                     : {'[]', unwrap('$1'), '_'}.
array -> STRING '[' oabody ']'              : {'[]', unwrap('$1'), '$3'}.

object -> '{' oabody '}'                    : {'{}', '_', '$2'}.
object -> STRING '{' '}'                    : {'{}', unwrap('$1'), '_'}.
object -> STRING '{' oabody '}'             : {'{}', unwrap('$1'), '$3'}.

oabody -> STRING                            : [unwrap('$1')].
oabody -> STRING '-' STRING                 : [{'-', unwrap('$1'), unwrap('$3')}].
oabody -> STRING ',' oabody                 : [unwrap('$1') | '$3'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Erlang code.

-behaviour(application).
-behaviour(supervisor).

% application callbacks
-export([start/2, stop/1]).

% Supervisor callbacks
-export([init/1]).

% parser and compiler interface
-export([parsetree/1]).

%%-----------------------------------------------------------------------------
%%                          dummy application interface
%%-----------------------------------------------------------------------------

start(_Type, _Args) -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
stop(_State)        -> ok.
init([])            -> {ok, { {one_for_one, 5, 10}, []} }.

%%-----------------------------------------------------------------------------
%%                          parser helper functions
%%-----------------------------------------------------------------------------

unwrap({_,_,X}) when is_list(X) ->
    case catch list_to_integer(X) of
        {'EXIT', _} -> list_to_binary(X);
        Integer -> Integer
    end.

%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%%                                  PARSER
%%-----------------------------------------------------------------------------
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
                    {parse_error, {Line, Module:format_error(Message), Toks}}
            end;
        LexErrorInfo -> {lex_error, jsonpath_lex:format_error(LexErrorInfo)}
    end;
parsetree(SomethingElse) -> {parse_error, {not_a_valid_json_path, SomethingElse}}.

%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%%                                  COMPILER
%%-----------------------------------------------------------------------------
%%-----------------------------------------------------------------------------


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%-----------------------------------------------------------------------------
%%                               EUnit test
%%-----------------------------------------------------------------------------

parse_test() ->
    ?debugMsg("==========================================="),
    ?debugMsg("|    J S O N   P A T H   P A R S I N G    |"),
    ?debugMsg("==========================================="),
    Cwd = filename:absname(""),
    {ShowParseTree, Tests} =
        case file:consult(filename:join([Cwd, "..", "test", "test.txt"])) of
            {ok, [show_parse_tree, T]}  -> {true, T};
            {ok, [T]}                   -> {true, T};
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
    {ok,Tokens,EndLine} = t_tokenize(Test),
    {ok, PTree} = t_parse(Tokens, EndLine),
    ?assertEqual(Target, PTree),
    ?debugFmt("~p", [PTree]),
    ?debugFmt("[~p]----------------------------------------",[N]),
    test_parse(N+1, ShowParseTree, Tests).

t_tokenize(Test) ->
    case jsonpath_lex:string(Test) of
        {ok,Tokens,EndLine} -> {ok,Tokens,EndLine};
        ErrorInfo ->
            ?assertEqual(ok, jsonpath_lex:format_error(ErrorInfo))
    end.

t_parse(Tokens, EndLine) ->
    case jpparse:parse(Tokens) of
        {ok, PTree} -> {ok, PTree};
        {error, {Line, Module, Message}} ->
            ?assertEqual(ok, {Line, Module:format_error(Message), {Tokens,EndLine}})
    end.

%%-----------------------------------------------------------------------------

-endif.
