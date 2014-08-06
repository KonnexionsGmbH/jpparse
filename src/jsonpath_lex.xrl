Definitions.

Rules.

% delimiters
[\:\(\)\[\]\{\}\-\$\,]    : {token, {list_to_atom(?debug(TokenChars)), TokenLine}}.
[A-Za-z0-9_]*           : {token, {'STRING', TokenLine, ?debug(TokenChars)}}.

Erlang code.

-define(debug(T), T).
%-define(debug(T), begin io:format(user, "Token ~p~n", [T]), T end).
