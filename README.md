#JPParse
##Introduction:


Json Path Parser parses a Json path into an abstract syntax tree, ready to be interpreted for matching them to json data objects.

###Still missing:

- Support for functions
- Support for unbound variables

##Glossary:
###For the path expression:

<dl>
<dt>Property</dt>
    <dd>key/attribute of a json object</dd>
<dt>Value</dt>
   <dd>Whatever data is associated with a property in a json object</dd>
<dt>':'</dt>
    <dd>Columns are used to access properties of objects (if an object has multiple identical properties, only the last one will match).</dd>
<dt>'[ ]'</dt>
    <dd>Square brackets signify you expect the data to be a list, and you wish to traverse it.<br>
    They can contain:
<ul>       
 <li>no value: meaning you wish to traverse the whole list.</li>
      <li>  a number: meaning you wish to only access the data on that index of the list</li>
       <li> a range (ex. 1-45): meaning you wish to traverse a sublist</li>
        <li>multiple numbers (ex 1,2,5): meaning you wish to access the data only on those index numbers</li></ul></dd>

<dt>'{ }'</dt>
    <dd>Curly braces signify you expect the data to be an object, and you wish to somehow traverse it<br>
    They can contain:
       <ul><li> no value: meaning you wish to traverse every value of the whole object</li>
        <li>a property: meaning you wish to access the value of that property (same as using a column). Only the last corresponding property will match.</li>
       <li> multiple properties: meaning you wish to access the values of those properties</li></ul></dd>

<dt>'$keys$'</dt>
   <dd> Signifies you wish to work on (or return) the object keys as if they were a list (containing the object's keys). Is understood as being a property.</dd>
   
<dt>'$values$'</dt>
   <dd> Signifies you wish to work on (or return) the object values as if they were a list (containing the object's values). Is understood as being a property.</dd>
   
<dt>'$firstChild$'</dt>
    <dd>Signifies you wish to work on the first value of the object, whatever the key. Is understood as being a property.</dd>
</dl>

####"single value / list of values as result" operator correspondence table
From the moment a path expression contains an operator who returns a list, result will be a list, even if only a single object matches.

input | output
--- | ---
`a:b` when `b` is list | List 
`a:b` when `b` is object | Single (object) 
`a:b` when `b` is neither list or object | Single (value) 
`a[]` | List 
`a[1]` | List 
`a[1-2]` | List 
`a[1,2]` | List 
`b{}` | List 
`b{prop}` | List 
`b{$keys$}` or `b:$keys$` | List
`b{$values$}` or `b:$values$` | List
`b{$firstChild$}` | List 
`b:$firstChild$` | Single 
`b{prop>propb}` | List 


###For the parsed form expression:
<dl>
<dt>'_'</dt>
<dd>    When used as second parameter in '{ }' or '[ ]' functions, signifies 'match all'
    When used as first parameter, signifies 'work on anonymous root data object' (used when the path expression starts with brackets or curly braces, or to access the root object).</dd>

<dt>'{}'</dt>
    <dd>Binary function. First parameter is the object to work on, second the attribute(s).
    It's the default operator: `{':', [a]}` is equal in meaning to `{':', [{'{}', '_', a}]}`, and should internally be treated as such.</dd>

<dt>'[]'</dt>
    <dd>Binary function: First parameter is the list to work on, second the index(es).</dd>

<dt>'-'</dt>
    <dd>Binary function: First parameter is the position to start from, second the position to stop at.</dd>

<dt>':'</dt>
    <dd>Unary function: Parameter list contains the path elements to traverse.</dd>

<dt>'$anything$</dt>
    <dd>Nullary function: translates to whatever the inclosed name may mean (currently supported: `firstChild`, `keys`, `values`)</dd>
       

##jpparse grammar/syntax examples:

path expression | parsed form
--- | ---
`<<"[1,3-4]:a">>` | `{':', [{'[]', '_', [1,{'-',3,4}]}, <<"a">>]}`
`<<"a:b[]:c">>` | `{':', [<<"a">>, {'[]', <<"b">>, '_'}, <<"c">>]}`
`<<"a:b[1]:c">>` | `{':', [<<"a">>, {'[]', <<"b">>, [1]}, <<"c">>]}`
`<<"a:b[1,2,3]">>` | `{':', [<<"a">>, {'[]', <<"b">>, [1,2,3]}]}`
`<<"a:b[1-20]:c">>` | `{':', [<<"a">>, {'[]', <<"b">>, [{'-', 1, 20}]}, <<"c">>]}`
`<<"a:b[1,2,9-20]:c">>` | `{':', [<<"a">>, {'[]', <<"b">>, [1,2,{'-',9,20}]}, <<"c">>]}`
`<<"{x,m-z}:a">>` | `{':', [{'{}', '_',[<<"x">>,{'-',<<"m">>,<<"z">>}]}, <<"a">>]}`
`<<"a:b{}:c">>` | `{':', [<<"a">>, {'{}', <<"b">>, '_'}, <<"c">>]}`
`<<"a:b{c}">>` | `{':', [<<"a">>, {'{}', <<"b">>, [<<"c">>]}]}`
`<<"a:b{c,d,m-z}">>` | `{':', [<<"a">>, {'{}', <<"b">>, [<<"c">>,<<"d">>,{'-',<<"m">>,<<"z">>}]}]}`
`<<"c{$firstChild$}">>` | `{':', [{'{}',<<"c">>,[{'$',<<"first_child">>}]}]}`
`<<"a:c:$keys$">>` | `{':', [<<"a">>, <<"c">>, {'$', <<"keys">>}]}`
`<<"a:b{$keys$}[1-2]">>` | `{':', [<<"a">>, <<"b">>, {'[]', {'{}',<<"b">>, {'$', <<"keys">>}}, {'-',1,2}}]}`
`<<"a:b[1-2]{}:c">>` | `{':', [<<"a">>, {'{}', {'[]', <<"b">>, {'-', 1, 2}},'_'},<<"c">>]}`
`<<"$keys$">> | `{':', [{'$',<<"keys">>]}`


###Example Usage

```erlang
1> jpparse:parsetree("a:b").
{ok,{{':',[<<"a">>,<<"b">>]},
     [{'STRING',1,"a"},{':',1},{'STRING',1,"b"}]}}
2> jpparse:parsetree("a:b{1-2"). 
{parse_error,{1,"syntax error before: ",
              [{'STRING',1,"a"},
               {':',1},
               {'STRING',1,"b"},
               {'{',1},
               {'STRING',1,"1"},
               {'-',1},
               {'STRING',1,"2"}]}}
3> jpparse:parsetree("a:b[1-2").
{parse_error,{1,"syntax error before: ",
              [{'STRING',1,"a"},
               {':',1},
               {'STRING',1,"b"},
               {'[',1},
               {'STRING',1,"1"},
               {'-',1},
               {'STRING',1,"2"}]}}
4> jpparse:parsetree("a:b[1-2]").
{ok,{{':',[<<"a">>,{'[]',<<"b">>,[{'-',1,2}]}]},
     [{'STRING',1,"a"},
      {':',1},
      {'STRING',1,"b"},
      {'[',1},
      {'STRING',1,"1"},
      {'-',1},
      {'STRING',1,"2"},
      {']',1}]}}
5> jpparse:parsetree("a:b[1,1-2]").
{ok,{{':',[<<"a">>,{'[]',<<"b">>,[1,{'-',1,2}]}]},
     [{'STRING',1,"a"},
      {':',1},
      {'STRING',1,"b"},
      {'[',1},
      {'STRING',1,"1"},
      {',',1},
      {'STRING',1,"1"},
      {'-',1},
      {'STRING',1,"2"},
      {']',1}]}}
```
