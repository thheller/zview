%%% @author Thomas Heller

Nonterminals
    Elements
    Literal

    ValueTag

    Value
    Variable
    Filter

    VarOrLiteral
    
    Tag
    TaggedIdentifier

    EndTag

    Block
    BlockTag

    FilterBlock
    FilterTag
    Filters

    ForBlock
    ForTag
    EmptyTag
    ForExpression
    ForGroup

    IfBlock
    IfTag
    IfExpression
    ElseTag

    ListExpr
    ListTail
    
    Args
    FilterArgs

    CommentBlock
    CommentTag

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
Elements -> Elements ValueTag : '$1' ++ ['$2'].

ValueTag -> open_var Value close_var : {output, '$2'}.

Value -> Value '|' Filter : {apply_filter, '$1', '$3'}.

Value -> Variable : '$1'.
Value -> Literal : '$1'.
Value -> ListExpr : '$1'.

Variable -> '$' identifier : {svariable, '$2'}.
Variable -> identifier : {variable, '$1'}.
Variable -> Variable '.' identifier : {attribute, {'$3', '$1'}}.

VarOrLiteral -> Literal : '$1'.
VarOrLiteral -> Variable : '$1'.

ListExpr -> '[' ']' : {list, list_end}.
ListExpr -> '[' VarOrLiteral ListTail : {list, {list_item, '$2', '$3'}}.

ListTail -> ']' : list_end.
ListTail -> ',' VarOrLiteral ListTail : {list_item, '$2', '$3'}.

Tag -> open_tag export_keyword Args close_tag: {export_tag, '$2', '$3'}.
Tag -> open_tag TaggedIdentifier Args close_tag : {tag, '$2', '$3'}.

EndTag -> open_tag end_keyword close_tag.

Block -> BlockTag Elements EndTag: {block, '$1', '$2'}.
BlockTag -> open_tag export_keyword Args do_keyword close_tag : {export_block, '$2', '$3'}.
BlockTag -> open_tag TaggedIdentifier Args do_keyword close_tag : {block_tag, '$2', '$3'}.

TaggedIdentifier -> identifier '@' identifier : {'$1', '$3'}.
TaggedIdentifier -> identifier : {default, '$1'}.

CommentBlock -> CommentTag Elements EndTag : {comment, '$2'}.
CommentTag -> open_tag comment_keyword close_tag.

FilterBlock -> FilterTag Elements EndTag : {filter, '$1', '$2'}.
FilterTag -> open_tag filter_keyword Filters close_tag : '$3'.

Filters -> Filter : ['$1'].
Filters -> Filters '|' Filter : '$1' ++ ['$3'].

ForBlock -> ForTag Elements EndTag : {for, '$1', '$2'}.
ForBlock -> ForTag Elements EmptyTag Elements EndTag : {for, '$1', '$2', '$4'}.
EmptyTag -> open_tag empty_keyword close_tag.
ForTag -> open_tag for_keyword ForExpression close_tag : {'$2', '$3'}.
ForExpression -> ForGroup in_keyword Variable : {'in', '$1', '$3'}.
ForGroup -> identifier : ['$1'].
ForGroup -> ForGroup ',' identifier : '$1' ++ ['$3'].

IfBlock -> IfTag Elements ElseTag Elements EndTag : {ifelse, '$1', '$2', '$4'}.
IfBlock -> IfTag Elements EndTag : {'if', '$1', '$2'}.
ElseTag -> open_tag else_keyword close_tag.

IfTag -> open_tag if_keyword IfExpression close_tag : '$3'.
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
