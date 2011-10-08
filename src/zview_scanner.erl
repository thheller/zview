%%%-------------------------------------------------------------------
%%% This file is a slightly modified Version of the Original at:
%%% File:      zview_scanner.erl
%%% Original:  https://github.com/evanmiller/erlydtl/blob/master/src/erlydtl_scanner.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @author    Thomas Heller <thheller@gmail.com>
%%%
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc 
%%% Template language scanner
%%% @end  
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon, Evan Miller
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%% @since 2007-11-11 by Roberto Saccon, Evan Miller
%%% @since 2011-09-25 modified by Thomas Heller
%%% Modifications:
%%% - Allow \t,\n in Expressions
%%% - Change Keywords: removed some, added: do, end, export
%%%-------------------------------------------------------------------
-module(zview_scanner).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').
-author('thheller@gmail.com').

-export([scan/1]). 

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec scan(T::template()) -> {ok, S::tokens()} | {error, Reason}
%% @type template() = string() | binary(). Template to parse
%% @type tokens() = [tuple()].
%% @doc Scan the template string T and return the a token list or
%% an error.
%% @end
%%--------------------------------------------------------------------
scan(Template) ->
  {ok, Scan} = scan(Template, [], {1, 1}, in_text),
  Cleaned = cleanup(Scan, []),
  {ok, Cleaned}.


cleanup([], Result) ->
  lists:reverse(Result);
cleanup([{string, SLoc, String}, {open_tag, _, _} = Tag | More], Result) ->
  cleanup(More, [Tag, {string, SLoc, strip_leading_newline(String)} | Result]);
cleanup([Other | More], Result) ->
  cleanup(More, [Other | Result]).

% that should be simpler?
strip_leading_newline(String) ->
  case string:strip(lists:reverse(String), left) of
    [$\n | More] ->
      lists:reverse(More);
    _ ->
      String
  end.

scan([], Scanned, _, in_text) ->
    {ok, lists:reverse(lists:map(
                fun
                    ({identifier, Pos, String}) ->
                        RevString = lists:reverse(String),
                        Keywords = [
                            "comment",
                            "filter",
                            "for", "in", "empty",
                            "if", "else", "elsif", "not", "or", "and", 
                            "export",
                            "do", "end"
                        ], 
                        Type = case lists:member(RevString, Keywords) of
                            true ->
                                list_to_atom(RevString ++ "_keyword");
                            _ ->
                                identifier
                        end,
                        {Type, Pos, list_to_atom(RevString)};
                    ({Category, Pos, String}) when  Category =:= string; 
                                                    Category =:= string_literal; 
                                                    Category =:= number_literal ->
                        {Category, Pos, lists:reverse(String)};
                    (Other) -> Other
                end, Scanned))};

scan([], _Scanned, _, {in_comment, _}) ->
    {error, "Reached end of file inside a comment."};

scan([], _Scanned, _, _) ->
    {error, "Reached end of file inside a code block."};

scan("<!--{{" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [{open_var, {Row, Column}, '<!--{{'} | Scanned], {Row, Column + length("<!--{{")}, {in_code, "}}-->"});

scan("{{" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [{open_var, {Row, Column}, '{{'} | Scanned], {Row, Column + length("{{")}, {in_code, "}}"});

scan("<!--{#" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, Scanned, {Row, Column + length("<!--{#")}, {in_comment, "#}-->"});

scan("{#" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, Scanned, {Row, Column + length("{#")}, {in_comment, "#}"});

scan("#}-->" ++ T, Scanned, {Row, Column}, {in_comment, "#}-->"}) ->
    scan(T, Scanned, {Row, Column + length("#}-->")}, in_text);

scan("#}" ++ T, Scanned, {Row, Column}, {in_comment, "#}"}) ->
    scan(T, Scanned, {Row, Column + length("#}")}, in_text);

scan("<!--{%" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [{open_tag, {Row, Column}, '<!--{%'} | Scanned], 
        {Row, Column + length("<!--{%")}, {in_code, "%}-->"});

scan("{%" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [{open_tag, {Row, Column}, '{%'} | Scanned], 
        {Row, Column + length("{%")}, {in_code, "%}"});

scan([_ | T], Scanned, {Row, Column}, {in_comment, Closer}) ->
    scan(T, Scanned, {Row, Column + 1}, {in_comment, Closer});

scan("\n" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, append_text_char(Scanned, {Row, Column}, $\n), {Row + 1, 1}, in_text);

scan([H | T], Scanned, {Row, Column}, in_text) ->
    scan(T, append_text_char(Scanned, {Row, Column}, H), {Row, Column + 1}, in_text);

scan("\"" ++ T, Scanned, {Row, Column}, {in_code, Closer}) ->
    scan(T, [{string_literal, {Row, Column}, "\""} | Scanned], {Row, Column + 1}, {in_double_quote, Closer});

scan("\"" ++ T, Scanned, {Row, Column}, {in_identifier, Closer}) ->
    scan(T, [{string_literal, {Row, Column}, "\""} | Scanned], {Row, Column + 1}, {in_double_quote, Closer});

scan("\'" ++ T, Scanned, {Row, Column}, {in_code, Closer}) ->
    scan(T, [{string_literal, {Row, Column}, "\""} | Scanned], {Row, Column + 1}, {in_single_quote, Closer});

scan("\'" ++ T, Scanned, {Row, Column}, {in_identifier, Closer}) ->
    scan(T, [{string_literal, {Row, Column}, "\""} | Scanned], {Row, Column + 1}, {in_single_quote, Closer});

scan([$\\ | T], Scanned, {Row, Column}, {in_double_quote, Closer}) ->
    scan(T, append_char(Scanned, $\\), {Row, Column + 1}, {in_double_quote_slash, Closer});

scan([H | T], Scanned, {Row, Column}, {in_double_quote_slash, Closer}) ->
    scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_double_quote, Closer});

scan([$\\ | T], Scanned, {Row, Column}, {in_single_quote, Closer}) ->
    scan(T, append_char(Scanned, $\\), {Row, Column + 1}, {in_single_quote_slash, Closer});

scan([H | T], Scanned, {Row, Column}, {in_single_quote_slash, Closer}) ->
    scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_single_quote, Closer});

% end quote
scan("\"" ++ T, Scanned, {Row, Column}, {in_double_quote, Closer}) ->
    scan(T, append_char(Scanned, 34), {Row, Column + 1}, {in_code, Closer});

% treat single quotes the same as double quotes
scan("\'" ++ T, Scanned, {Row, Column}, {in_single_quote, Closer}) ->
    scan(T, append_char(Scanned, 34), {Row, Column + 1}, {in_code, Closer});

scan([H | T], Scanned, {Row, Column}, {in_double_quote, Closer}) ->
    scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_double_quote, Closer});

scan([H | T], Scanned, {Row, Column}, {in_single_quote, Closer}) ->
    scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_single_quote, Closer});


scan("}}-->" ++ T, Scanned, {Row, Column}, {_, "}}-->"}) ->
    scan(T, [{close_var, {Row, Column}, '}}-->'} | Scanned], 
        {Row, Column + length("}}-->")}, in_text);

scan("}}" ++ T, Scanned, {Row, Column}, {_, "}}"}) ->
    scan(T, [{close_var, {Row, Column}, '}}'} | Scanned], {Row, Column + 2}, in_text);

scan("%}-->" ++ T, Scanned, {Row, Column}, {_, "%}-->"}) ->
    scan(T, [{close_tag, {Row, Column}, '%}-->'} | Scanned], 
        {Row, Column + length("%}-->")}, in_text);

scan("%}" ++ T, Scanned, {Row, Column}, {_, "%}"}) ->
    scan(T, [{close_tag, {Row, Column}, '%}'} | Scanned], 
        {Row, Column + 2}, in_text);

scan("==" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'==', {Row, Column}} | Scanned], {Row, Column + 2}, {in_code, Closer});

scan("!=" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'!=', {Row, Column}} | Scanned], {Row, Column + 2}, {in_code, Closer});

scan(">=" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'>=', {Row, Column}} | Scanned], {Row, Column + 2}, {in_code, Closer});

scan("<=" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'<=', {Row, Column}} | Scanned], {Row, Column + 2}, {in_code, Closer});

scan("<" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'<', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan(">" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'>', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("("++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'(', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan(")" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{')', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("," ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{',', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("|" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'|', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("=" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'=', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan(":" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{':', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("$" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'$', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("[" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'[', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("]" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{']', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("@" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
   scan(T, [{'@', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("." ++ T, Scanned, {Row, Column}, {_, Closer}) ->
   scan(T, [{'.', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("_(" ++ T, Scanned, {Row, Column}, {in_code, Closer}) ->
    scan(T, lists:reverse([{'_', {Row, Column}}, {'(', {Row, Column + 1}}], Scanned), {Row, Column + 2}, {in_code, Closer});

scan("\n" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, Scanned, {Row + 1, Column + 1}, {in_code, Closer});

scan("\t" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, Scanned, {Row, Column + 1}, {in_code, Closer});

scan(" " ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, Scanned, {Row, Column + 1}, {in_code, Closer});


scan([H | T], Scanned, {Row, Column}, {in_code, Closer}) ->
    case char_type(H) of
        X when X =:= letter_underscore ->
            scan(T, [{identifier, {Row, Column}, [H]} | Scanned], {Row, Column + 1}, {in_identifier, Closer});
        digit ->
            scan(T, [{number_literal, {Row, Column}, [H]} | Scanned], {Row, Column + 1}, {in_number, Closer});
        _ ->
            {error, {Row, ?MODULE, lists:concat(["Illegal character in column ", Column])}}
    end;

scan([H | T], Scanned, {Row, Column}, {in_number, Closer}) ->
    case char_type(H) of
        digit ->
            scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_number, Closer});
        _ ->
            {error, {Row, ?MODULE, lists:concat(["Illegal character in Number at column ", Column])}}
    end;

scan([H | T], Scanned, {Row, Column}, {in_identifier, Closer}) ->
    case char_type(H) of
        X when X =:= letter_underscore; X =:= digit ->
            scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_identifier, Closer});
        _ ->
            {error, {Row, ?MODULE, lists:concat(["Illegal character in Identifier at column ", Column])}}
    end.

% internal functions

append_char([{Type, Pos, Chars}|Scanned], Char) ->
    [{Type, Pos, [Char | Chars]} | Scanned].

append_text_char([], {Row, Column}, Char) ->
    [{string, {Row, Column}, [Char]}];
append_text_char([{string, StrPos, Chars} |Scanned1], _, Char) ->
    [{string, StrPos, [Char | Chars]} | Scanned1];
append_text_char(Scanned, {Row, Column}, Char) ->
    [{string, {Row, Column}, [Char]} | Scanned].

char_type(C) when ((C >= $a) andalso (C =< $z)) orelse ((C >= $A) andalso (C =< $Z)) orelse (C == $_) ->
    letter_underscore;
char_type($$) ->
    dollar; 
char_type(C) when ((C >= $0) andalso (C =< $9)) ->
    digit;
char_type(_C) ->
    undefined.


-ifdef(TEST).

var_ident_test() ->
  {ok, Result} = scan("{{ [\"a\", hello.world] | bla@wtf: a, 'x' }}"),
  ?debugVal(Result),
  ok.

cleanup_test() ->
  {ok, Result} = scan(
    "line first
    {% for x in $vars.parent %}     
      {{ $for.index }}
      {{ x }}
    {% endfor %}    
    line after"),

  ?debugVal(Result),
  ok.

-endif.
