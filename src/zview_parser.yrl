%%%-------------------------------------------------------------------
%%% File:      erlydtl_parser.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc Template language grammar
%%% @reference  See <a href="http://erlydtl.googlecode.com" target="_top">http://erlydtl.googlecode.com</a> for more information
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
%%%-------------------------------------------------------------------

Nonterminals
    Elements
    Literal

    ValueBraced

    Value
    Variable
    Filter

    VarOrLiteral
    
    Tag
    TaggedIdentifier

    Block
    BlockBraced
    EndBlockBraced

    FilterBlock
    FilterBraced
    EndFilterBraced
    Filters

    ForBlock
    ForBraced
    EmptyBraced
    EndForBraced
    ForExpression
    ForGroup

    IfBlock
    IfBraced
    IfExpression
    ElseBraced
    EndIfBraced

    ListExpr
    ListTail
    
    Args
    FilterArgs

    CommentBlock
    CommentBraced
    EndCommentBraced

    Unot.

Terminals
    and_keyword
    do_keyword
    end_keyword
    close_tag
    close_var
    comment_keyword
    else_keyword
    empty_keyword
    endfilter_keyword
    endfor_keyword
    endif_keyword
    endcomment_keyword
    export_keyword
    filter_keyword
    for_keyword
    identifier
    if_keyword
    in_keyword
    not_keyword
    number_literal
    or_keyword
    open_tag
    open_var
    string_literal
    string
    ',' '|' '=' ':' '.'
    '==' '!='
    '>=' '<='
    '>' '<'
    '(' ')' '@'
    '$' '[' ']'
    '_'.

Rootsymbol
    Elements.

%% Operator precedences for the E non terminal
Left 100 or_keyword.
Left 110 and_keyword.
Nonassoc 300 '==' '!=' '>=' '<=' '>' '<'.
Unary 600 Unot.

Elements -> '$empty' : [].
Elements -> Elements string : '$1' ++ ['$2'].
Elements -> Elements CommentBlock : '$1' ++ ['$2'].
Elements -> Elements Tag : '$1' ++ ['$2'].
Elements -> Elements Block : '$1' ++ ['$2'].
Elements -> Elements FilterBlock : '$1' ++ ['$2'].
Elements -> Elements ForBlock : '$1' ++ ['$2'].
Elements -> Elements IfBlock : '$1' ++ ['$2'].
Elements -> Elements ValueBraced : '$1' ++ ['$2'].

ValueBraced -> open_var Value close_var : {output, '$2'}.

Value -> Value '|' Filter : {apply_filter, '$1', '$3'}.

Value -> Variable : '$1'.
Value -> Literal : '$1'.
Value -> ListExpr : '$1'.

Variable -> '$' identifier : {svariable, '$2'}.
Variable -> identifier : {variable, '$1'}.
Variable -> Variable '.' identifier : {attribute, {'$3', '$1'}}.

VarOrLiteral -> Literal : '$1'.
VarOrLiteral -> Variable : '$1'.

ListExpr -> '[' ']' : empty_list.
ListExpr -> '[' VarOrLiteral ListTail : {list, {list_item, '$2', '$3'}}.

ListTail -> ']' : list_end.
ListTail -> ',' VarOrLiteral ListTail : {list_item, '$2', '$3'}.

Tag -> open_tag export_keyword Args close_tag: {export_tag, '$2', '$3'}.
Tag -> open_tag TaggedIdentifier Args close_tag : {tag, '$2', '$3'}.

Block -> BlockBraced Elements EndBlockBraced: {block, '$1', '$2'}.
BlockBraced -> open_tag export_keyword Args do_keyword close_tag : {export_block, '$2', '$3'}.
BlockBraced -> open_tag TaggedIdentifier Args do_keyword close_tag : {block_tag, '$2', '$3'}.
EndBlockBraced -> open_tag end_keyword close_tag.

TaggedIdentifier -> identifier '@' identifier : {'$1', '$3'}.
TaggedIdentifier -> identifier : {default, '$1'}.

CommentBlock -> CommentBraced Elements EndCommentBraced : {comment, '$2'}.
CommentBraced -> open_tag comment_keyword close_tag.
EndCommentBraced -> open_tag endcomment_keyword close_tag.

FilterBlock -> FilterBraced Elements EndFilterBraced : {filter, '$1', '$2'}.
FilterBraced -> open_tag filter_keyword Filters close_tag : '$3'.
EndFilterBraced -> open_tag endfilter_keyword close_tag.

Filters -> Filter : ['$1'].
Filters -> Filters '|' Filter : '$1' ++ ['$3'].

ForBlock -> ForBraced Elements EndForBraced : {for, '$1', '$2'}.
ForBlock -> ForBraced Elements EmptyBraced Elements EndForBraced : {for, '$1', '$2', '$4'}.
EmptyBraced -> open_tag empty_keyword close_tag.
ForBraced -> open_tag for_keyword ForExpression close_tag : {'$2', '$3'}.
EndForBraced -> open_tag endfor_keyword close_tag.
ForExpression -> ForGroup in_keyword Variable : {'in', '$1', '$3'}.
ForGroup -> identifier : ['$1'].
ForGroup -> ForGroup ',' identifier : '$1' ++ ['$3'].

IfBlock -> IfBraced Elements ElseBraced Elements EndIfBraced : {ifelse, '$1', '$2', '$4'}.
IfBlock -> IfBraced Elements EndIfBraced : {'if', '$1', '$2'}.
IfBraced -> open_tag if_keyword IfExpression close_tag : '$3'.
IfExpression -> Value in_keyword Value : {'expr', 'in', '$1', '$3'}.
IfExpression -> Value not_keyword in_keyword Value : {'expr', 'not', {'expr', 'in', '$1', '$4'}}.
IfExpression -> Value '==' Value : {'expr', 'eq', '$1', '$3'}.
IfExpression -> Value '!=' Value : {'expr', 'ne', '$1', '$3'}.
IfExpression -> Value '>=' Value : {'expr', 'ge', '$1', '$3'}.
IfExpression -> Value '<=' Value : {'expr', 'le', '$1', '$3'}.
IfExpression -> Value '>' Value : {'expr', 'gt', '$1', '$3'}.
IfExpression -> Value '<' Value : {'expr', 'lt', '$1', '$3'}.
IfExpression -> '(' IfExpression ')' : '$2'.
IfExpression -> Unot : '$1'.
IfExpression -> IfExpression or_keyword IfExpression : {'expr', 'or', '$1', '$3'}.
IfExpression -> IfExpression and_keyword IfExpression : {'expr', 'and', '$1', '$3'}.
IfExpression -> Value : '$1'.

Unot -> not_keyword IfExpression : {expr, 'not', '$2'}.

ElseBraced -> open_tag else_keyword close_tag.
EndIfBraced -> open_tag endif_keyword close_tag.

Filter -> TaggedIdentifier FilterArgs : {filter, '$1', '$2'}.

FilterArgs -> '$empty' : [].
FilterArgs -> FilterArgs ':' Variable : '$1' ++ ['$3'].
FilterArgs -> FilterArgs ':' Literal : '$1' ++ ['$3'].
FilterArgs -> FilterArgs ',' Variable : '$1' ++ ['$3'].
FilterArgs -> FilterArgs ',' Literal : '$1' ++ ['$3'].

Literal -> '_' '(' string_literal ')' : {trans, '$3'}.
Literal -> string_literal : '$1'.
Literal -> number_literal : '$1'.

Args -> '$empty' : [].
Args -> Args identifier : '$1' ++ [{'auto', '$2'}].
Args -> Args identifier '=' Value : '$1' ++ [{'$2', '$4'}].
