%% -*- erlang -*-
Definitions.

Rules.

% delimiters
(\:\:)                              : {token, {list_to_atom(?debug(TokenChars)), TokenLine}}.
[\:\(\)\[\]\{\}\#\,\|\-\+\*\/\\%]   : {token, {list_to_atom(?debug(TokenChars)), TokenLine}}.
% [A-Za-z0-9_@\$\.]*                  : {token, {'STRING', TokenLine, ?debug(TokenChars)}}.
[A-Za-z][A-Za-z0-9_@\$]*            : {token, {'STRING', TokenLine, ?debug(TokenChars)}}.
(\"((\$|[^\"]*)*(\"\")*)*\")        : {token, {'STRING', TokenLine, ?debug(TokenChars)}}.

([\s\t\r\n]+)                       : skip_token.    %% white space

Erlang code.

-define(debug(T), T).
%-define(debug(T), begin io:format(user, "Token ~p~n", [T]), T end).
