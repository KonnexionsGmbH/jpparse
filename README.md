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

<table>
      <tbody>
        <tr>
          <td valign="top"><big><tt>a:b when b is list
              </tt></big></td>
          <td valign="top"><big><tt>List
              </tt></big></td>
        </tr>
        <tr>
          <td valign="top"><big><tt>a:b when b is object
              </tt></big></td>
          <td valign="top"><big><tt>Single (object)
              </tt></big></td>
        </tr>
        <tr>
          <td valign="top"><big><tt>a:b when b is neither list or object
              </tt></big></td>
          <td valign="top"><big><tt>Single (value)
              </tt></big></td>
        </tr>
        <tr>
          <td valign="top"><big><tt>a[]
              </tt></big></td>
          <td valign="top"><big><tt>List
              </tt></big></td>
        </tr>
        <tr>
          <td valign="top"><big><tt>a[1]
              </tt></big></td>
          <td valign="top"><big><tt>List
              </tt></big></td>
        </tr>
        <tr>
          <td valign="top"><big><tt>a[1-2]
              </tt></big></td>
          <td valign="top"><big><tt>List
              </tt></big></td>
        </tr>
        <tr>
          <td valign="top"><big><tt>a[1,2]
              </tt></big></td>
          <td valign="top"><big><tt>List
              </tt></big></td>
        </tr>
        <tr>
          <td valign="top"><big><tt>b{}
              </tt></big></td>
          <td valign="top"><big><tt>List
              </tt></big></td>
        </tr>
        <tr>
          <td valign="top"><big><tt>b{prop}
              </tt></big></td>
          <td valign="top"><big><tt>List
              </tt></big></td>
        </tr>
        <tr>
          <td valign="top"><big><tt>b{$keys$} or b:$keys$</tt></big></td>
          <td valign="top"><big><tt>List</tt></big></td>
        </tr>
         <tr>
          <td valign="top"><big><tt>b{$values$} or b:$values$</tt></big></td>
          <td valign="top"><big><tt>List</tt></big></td>
        </tr>
        <tr>
          <td valign="top"><big><tt>b{$first-child$}
              </tt></big></td>
          <td valign="top"><big><tt>List
              </tt></big></td>
        </tr>
        <tr>
          <td valign="top"><big><tt>b:$first-child$
              </tt></big></td>
          <td valign="top"><big><tt>Single
              </tt></big></td>
        </tr>
        <tr>
          <td valign="top"><big><tt>b{prop<<"a">>propb}
              </tt></big></td>
          <td valign="top"><big><tt>List
              </tt></big></td>
        </tr>
      </tbody>
    </table>


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
<table>
      <tbody>
        <tr>
          <th valign="top">path expression
          </th>
          <th valign="top">parsed form
          </th>
        </tr>
        <tr>
          <td valign="top"><big><tt>&lt;&lt;"a:b[1]:c"&gt;&gt;
              </tt></big></td>
          <td valign="top"><big><tt>{':', [&lt;&lt;"a"&gt;&gt;, {'[]', &lt;&lt;"b"&gt;&gt;, 1}, &lt;&lt;"c"&gt;&gt;]}
              </tt></big></td>
        </tr>
        <tr>
          <td valign="top"><big><tt>&lt;&lt;"a:b[1-20]"&gt;&gt;
              </tt></big></td>
          <td valign="top"><big><tt>{':', [&lt;&lt;"a"&gt;&gt;, {'[]', &lt;&lt;"b"&gt;&gt;, {'-', 1, 20}}, &lt;&lt;"c"&gt;&gt;]}
              </tt></big></td>
        </tr>
        <tr>
          <td valign="top"><big><tt>&lt;&lt;"a:b[1,2,3]"&gt;&gt;
              </tt></big></td>
          <td valign="top"><big><tt>{':', [&lt;&lt;"a"&gt;&gt;, {'[]', &lt;&lt;"b"&gt;&gt;, [1,2,3]}]}</tt></big></td>
        </tr>
        <tr>
          <td valign="top"><big><tt>&lt;&lt;"a:b{}:c"&gt;&gt;
              </tt></big></td>
          <td valign="top"><big><tt>{':', [&lt;&lt;"a"&gt;&gt;, {'{}', &lt;&lt;"b"&gt;&gt;, []}, &lt;&lt;"c"&gt;&gt;]}</tt></big></td>
        </tr>
        <tr>
          <td valign="top"><big><tt>&lt;&lt;"a:b{c}"&gt;&gt;
          </tt></big></td>
          <td valign="top"><big><tt>{':', [&lt;&lt;"a"&gt;&gt;, {'{}', &lt;&lt;"b"&gt;&gt;, &lt;&lt;"c"&gt;&gt;}]}</tt></big></td>
        </tr>
        <tr>
          <td valign="top"><big><tt>&lt;&lt;"a:b[]:c"&gt;&gt;
          </tt></big></td>
          <td valign="top"><tt><big>{':', [&lt;&lt;"a"&gt;&gt;, {'[]', &lt;&lt;"b"&gt;&gt;, []}, &lt;&lt;"c"&gt;&gt;]}</big></tt></td>
        </tr>
        <tr>
          <td valign="top"><big><tt>&lt;&lt;"[]:c{$firstChild$}:d"&gt;&gt;
          </tt></big></td>
          <td valign="top"><big><tt>{':', [{'[]', '\_', []}, {'{}', &lt;&lt;"c"&gt;&gt;,
                {'$',&lt;&lt;""firstChild"&gt;&gt;}}, &lt;&lt;"d"&gt;&gt;]}
              </tt></big></td>
        </tr>
         <tr>
          <td valign="top"><big><tt>&lt;&lt;"c{$values$}:d"&gt;&gt;
          </tt></big></td>
          <td valign="top"><big><tt>{':', [{'{}', &lt;&lt;"c"&gt;&gt;,
                {'$',&lt;&lt;""values"&gt;&gt;}}, &lt;&lt;"d"&gt;&gt;]}
              </tt></big></td>
        </tr>
        <tr>
          <td valign="top"><big><tt>&lt;&lt;"c:{$values$}"&gt;&gt;
          </tt></big></td>
          <td valign="top"><big><tt>{':', [&lt;&lt;"c"&gt;&gt;, {'$',&lt;&lt;""values"&gt;&gt;}}]}
              </tt></big></td>
        </tr>
        <tr>
          <td valign="top"><big><tt>&lt;&lt;"[1]:d"&gt;&gt;
              </tt></big></td>
          <td valign="top"><big><tt>{':', [{'[]', '\_', 1}, &lt;&lt;"d"&gt;&gt;]}
              </tt></big></td>
        </tr>
        <tr>
          <td valign="top"><big><tt>&lt;&lt;"{c,d}:e"&gt;&gt;
              </tt></big></td>
          <td valign="top"><big><tt>{':', [{'{}', '\_', [&lt;&lt;"c"&gt;&gt;, &lt;&lt;"d"&gt;&gt;]},&lt;&lt;"e"&gt;&gt;]}
              </tt></big></td>
        </tr>
        <tr>
          <td valign="top"><big><tt>&lt;&lt;"a:b{c,d}:e"&gt;&gt;
              </tt></big></td>
          <td valign="top"><big><tt>{':', [&lt;&lt;"a"&gt;&gt;, {'{}', &lt;&lt;"b"&gt;&gt;, [&lt;&lt;"c"&gt;&gt;, &lt;&lt;"d"&gt;&gt;]}, &lt;&lt;"e"&gt;&gt;]}
              </tt></big></td>
        </tr>
        <tr>
          <td valign="top"><big><tt>&lt;&lt;"a:c:$keys$"&gt;&gt;
              </tt></big></td>
          <td valign="top"><big><tt>{':', [&lt;&lt;"a"&gt;&gt;, &lt;&lt;"c"&gt;&gt;, '$keys$']}
              </tt></big></td>
        </tr>
        <tr>
          <td valign="top"><big><tt>&lt;&lt;"a:b{$keys$}[1-2]"&gt;&gt;
          </tt></big></td>
          <td valign="top"><big><tt>{':', [&lt;&lt;"a"&gt;&gt;, &lt;&lt;"b"&gt;&gt;, {'[]',
                {'{}',&lt;&lt;"b"&gt;&gt;,'$keys$'}, {'-', 1, 2}}]}</tt></big></td>
        </tr>
        <tr>
          <td valign="top"><big><tt>&lt;&lt;"a:b[1-2]{}:c"&gt;&gt;
              </tt></big></td>
          <td valign="top"><big><tt>{':', [&lt;&lt;"a"&gt;&gt;, {'{}', {'[]', &lt;&lt;"b"&gt;&gt;, {'-', 1,
                2}}, []}, &lt;&lt;"c"&gt;&gt;]}
              </tt></big></td>
        </tr>
      </tbody>
    </table>
