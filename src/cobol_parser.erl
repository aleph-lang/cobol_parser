%% Module parser COBOL compatible avec AlephTree (Rust)
-module(cobol_parser).
-export([parse/1, parse_file/1, tokenize/1, to_json/1]).

%% Records pour les structures COBOL spécifiques
-record(cobol_program, {
    identification,
    environment,
    data_division,
    procedure_division
}).

-record(cobol_identification, {
    program_id,
    author,
    date_written
}).

-record(cobol_data_division, {
    working_storage = [],
    file_section = []
}).

-record(cobol_variable, {
    level,
    name,
    type,
    value,
    occurs
}).

-record(cobol_procedure_division, {
    paragraphs = []
}).

-record(cobol_paragraph, {
    name,
    statements = []
}).

%% Point d'entrée principal
parse_file(Filename) ->
    {ok, Content} = file:read_file(Filename),
    parse(binary_to_list(Content)).

parse(CobolSource) ->
    Tokens = tokenize(CobolSource),
    case parse_tokens(Tokens) of
        {ok, CobolAST} -> 
            AlephAST = cobol_to_aleph(CobolAST),
            {ok, AlephAST};
        Error -> Error
    end.

%% Conversion vers AlephTree
cobol_to_aleph(#cobol_program{
    identification = Id,
    data_division = DataDiv,
    procedure_division = ProcDiv
}) ->
    ProgramIdNode = case Id of
        #cobol_identification{program_id = PId} when PId =/= undefined ->
            aleph_comment("PROGRAM-ID: " ++ atom_to_list(PId));
        _ -> aleph_unit()
    end,
    
    DataNodes = case DataDiv of
        #cobol_data_division{working_storage = WS} ->
            [convert_variable_to_aleph(Var) || Var <- WS];
        _ -> []
    end,
    
    ProcNodes = case ProcDiv of
        #cobol_procedure_division{paragraphs = Pars} ->
            lists:flatten([convert_paragraph_to_aleph(Par) || Par <- Pars]);
        _ -> []
    end,
    
    AllNodes = [ProgramIdNode] ++ DataNodes ++ ProcNodes,
    case AllNodes of
        [Single] -> Single;
        [First | Rest] -> 
            lists:foldl(fun(Node, Acc) -> 
                aleph_stmts(Acc, Node) 
            end, First, Rest)
    end.

convert_variable_to_aleph(#cobol_variable{
    level = Level,
    name = Name,
    type = Type,
    value = Value
}) ->
    VarName = atom_to_list(Name),
    InitValue = case Value of
        undefined -> aleph_unit();
        {string, StrVal} -> aleph_string(StrVal);
        {number, NumVal} -> aleph_int(integer_to_list(NumVal));
        _ -> aleph_unit()
    end,
    
    Comment = aleph_comment(io_lib:format("~p ~s PIC ~s", [Level, VarName, format_type(Type)])),
    
    case InitValue of
        #{<<"type">> := <<"Unit">>} -> Comment;
        _ -> 
            LetNode = aleph_let(VarName, "false", InitValue, aleph_unit()),
            aleph_stmts(Comment, LetNode)
    end.

convert_paragraph_to_aleph(#cobol_paragraph{name = Name, statements = Stmts}) ->
    ParComment = aleph_comment("PARAGRAPH: " ++ atom_to_list(Name)),
    StmtNodes = [convert_statement_to_aleph(Stmt) || Stmt <- Stmts],
    case StmtNodes of
        [] -> [ParComment];
        _ -> [ParComment | StmtNodes]
    end.

convert_statement_to_aleph({move_stmt, Source, Target}) ->
    SourceNode = convert_expr_to_aleph(Source),
    TargetName = case Target of
        {identifier, Name} -> atom_to_list(Name);
        Name when is_atom(Name) -> atom_to_list(Name);
        Name when is_list(Name) -> Name
    end,
    aleph_let(TargetName, "false", SourceNode, aleph_unit());

convert_statement_to_aleph({display_stmt, Items}) ->
    ItemNodes = [convert_expr_to_aleph(Item) || Item <- Items],
    case ItemNodes of
        [Single] -> aleph_app("DISPLAY", aleph_ident("display"), [Single]);
        Multiple -> aleph_app("DISPLAY", aleph_ident("display"), Multiple)
    end;

convert_statement_to_aleph({accept_stmt, Target}) ->
    TargetName = case Target of
        {identifier, Name} -> atom_to_list(Name);
        Name when is_atom(Name) -> atom_to_list(Name);
        Name when is_list(Name) -> Name
    end,

    InputValue = aleph_app("INPUT", aleph_ident("input"), []),
    aleph_let(TargetName, "false", InputValue, aleph_unit());

convert_statement_to_aleph({if_stmt, Condition, ThenClause, ElseClause}) ->
    CondNode = convert_expr_to_aleph(Condition),
    ThenNode = case ThenClause of
        [] -> aleph_unit();
        [Single] -> convert_statement_to_aleph(Single);
        Multiple -> 
            ConvertedStmts = [convert_statement_to_aleph(S) || S <- Multiple],
            lists:foldl(fun(Node, Acc) -> aleph_stmts(Acc, Node) end, 
                       hd(ConvertedStmts), tl(ConvertedStmts))
    end,
    ElseNode = case ElseClause of
        [] -> aleph_unit();
        ElseStmts -> 
            ConvertedElse = [convert_statement_to_aleph(S) || S <- ElseStmts],
            lists:foldl(fun(Node, Acc) -> aleph_stmts(Acc, Node) end, 
                       hd(ConvertedElse), tl(ConvertedElse))
    end,
    aleph_if(CondNode, ThenNode, ElseNode);

convert_statement_to_aleph({perform_stmt, Target, Times}) ->
    TargetName = case Target of
        {identifier, Name} -> atom_to_list(Name);
        Name when is_atom(Name) -> atom_to_list(Name);
        Name when is_list(Name) -> Name
    end,
    
    case Times of
        undefined -> aleph_app("PERFORM", aleph_ident("perform"), [aleph_ident(TargetName)]);
        N when is_integer(N) ->
            CounterVar = "PERFORM_COUNTER",
            InitExpr = aleph_let(CounterVar, "false", aleph_int("0"), aleph_unit()),
            Condition = aleph_le(aleph_var(CounterVar, "false"), aleph_int(integer_to_list(N))),
            LoopExpr = aleph_app("PERFORM", aleph_ident("perform"), [aleph_ident(TargetName)]),
            PostExpr = aleph_let(CounterVar, "false", 
                         aleph_add(aleph_var(CounterVar, "false"), aleph_int("1")), 
                         aleph_unit()),
            aleph_while(InitExpr, Condition, LoopExpr, PostExpr)
    end;

convert_statement_to_aleph(stop_run) ->
    aleph_app("STOP_RUN", aleph_ident("stop_run"), []);

convert_statement_to_aleph(Stmt) ->
    aleph_comment("UNKNOWN STATEMENT: " ++ io_lib:format("~p", [Stmt])).

convert_expr_to_aleph({identifier, Name}) ->
    aleph_var(atom_to_list(Name), "false");
convert_expr_to_aleph({string, Value}) ->
    aleph_string(Value);
convert_expr_to_aleph({number, Value}) ->
    aleph_int(integer_to_list(Value));
convert_expr_to_aleph({condition, Left, Op, Right}) ->
    LeftNode = convert_expr_to_aleph(Left),
    RightNode = convert_expr_to_aleph(Right),
    case Op of
        {identifier, "="} -> aleph_eq(LeftNode, RightNode);
        {identifier, "<"} -> aleph_le(LeftNode, RightNode);
        {identifier, ">"} -> aleph_le(RightNode, LeftNode);
        _ -> aleph_eq(LeftNode, RightNode)  % Défaut
    end;
convert_expr_to_aleph(Expr) when is_atom(Expr) ->
    aleph_var(atom_to_list(Expr), "false");
convert_expr_to_aleph(Expr) when is_list(Expr) ->
    aleph_string(Expr);
convert_expr_to_aleph(Expr) when is_integer(Expr) ->
    aleph_int(integer_to_list(Expr));
convert_expr_to_aleph(Expr) ->
    aleph_comment("UNKNOWN EXPR: " ++ io_lib:format("~p", [Expr])).

%% Fonctions de construction des nœuds AlephTree
aleph_unit() ->
    #{<<"type">> => <<"Unit">>}.

aleph_int(Value) ->
    #{<<"type">> => <<"Int">>, <<"value">> => list_to_binary(Value)}.

aleph_string(Value) ->
    #{<<"type">> => <<"String">>, <<"value">> => list_to_binary(Value)}.

aleph_ident(Value) ->
    #{<<"type">> => <<"Ident">>, <<"value">> => list_to_binary(Value)}.

aleph_var(Var, IsPointer) ->
    #{<<"type">> => <<"Var">>, 
      <<"var">> => list_to_binary(Var), 
      <<"isPointer">> => list_to_binary(IsPointer)}.

aleph_let(Var, IsPointer, Value, Expr) ->
    #{<<"type">> => <<"Let">>,
      <<"var">> => list_to_binary(Var),
      <<"isPointer">> => list_to_binary(IsPointer),
      <<"value">> => Value,
      <<"expr">> => Expr}.

aleph_if(Condition, Then, Else) ->
    #{<<"type">> => <<"If">>,
      <<"condition">> => Condition,
      <<"then">> => Then,
      <<"else">> => Else}.

aleph_while(InitExpr, Condition, LoopExpr, PostExpr) ->
    #{<<"type">> => <<"While">>,
      <<"initExpr">> => InitExpr,
      <<"condition">> => Condition,
      <<"loopExpr">> => LoopExpr,
      <<"postExpr">> => PostExpr}.

aleph_add(Expr1, Expr2) ->
    #{<<"type">> => <<"Add">>,
      <<"numberExpr1">> => Expr1,
      <<"numberExpr2">> => Expr2}.

aleph_eq(Expr1, Expr2) ->
    #{<<"type">> => <<"Eq">>,
      <<"expr1">> => Expr1,
      <<"expr2">> => Expr2}.

aleph_le(Expr1, Expr2) ->
    #{<<"type">> => <<"LE">>,
      <<"expr1">> => Expr1,
      <<"expr2">> => Expr2}.

aleph_app(ObjectName, Fun, ParamList) ->
    #{<<"type">> => <<"App">>,
      <<"objectName">> => list_to_binary(ObjectName),
      <<"fun">> => Fun,
      <<"paramList">> => ParamList}.

aleph_stmts(Expr1, Expr2) ->
    #{<<"type">> => <<"Stmts">>,
      <<"expr1">> => Expr1,
      <<"expr2">> => Expr2}.

aleph_comment(Value) ->
    #{<<"type">> => <<"Comment">>, 
      <<"value">> => list_to_binary(Value)}.

%% Fonction pour exporter en JSON
to_json(AlephTree) ->
    jsx:encode(AlephTree, [{space, 2}, {indent, 2}]).

%% Utilitaires
format_type(undefined) -> "UNKNOWN";
format_type(Type) when is_list(Type) -> Type;
format_type(Type) -> io_lib:format("~p", [Type]).

%% Tokenizer basique
tokenize(Source) ->
    Lines = string:tokens(Source, "\n"),
    ProcessedLines = [process_line(Line) || Line <- Lines],
    FlatTokens = lists:flatten(ProcessedLines),
    [Token || Token <- FlatTokens, Token =/= skip].

process_line(Line) ->
    TrimmedLine = string:strip(Line),
    case TrimmedLine of
        [] -> [skip];
        [$* | _] -> [skip];  % Commentaire
        _ -> tokenize_line(TrimmedLine)
    end.

tokenize_line(Line) ->
    Tokens = string:tokens(string:to_upper(Line), " \t."),
    [classify_token(Token) || Token <- Tokens].

classify_token(Token) ->
    case Token of
        "IDENTIFICATION" -> {keyword, identification};
        "DIVISION" -> {keyword, division};
        "PROGRAM-ID" -> {keyword, program_id};
        "AUTHOR" -> {keyword, author};
        "DATE-WRITTEN" -> {keyword, date_written};
        "ENVIRONMENT" -> {keyword, environment};
        "DATA" -> {keyword, data};
        "WORKING-STORAGE" -> {keyword, working_storage};
        "FILE" -> {keyword, file};
        "SECTION" -> {keyword, section};
        "PROCEDURE" -> {keyword, procedure};
        "MOVE" -> {keyword, move};
        "TO" -> {keyword, to};
        "DISPLAY" -> {keyword, display};
        "ACCEPT" -> {keyword, accept};
        "IF" -> {keyword, cobol_if};  % Éviter le mot-clé réservé 'if'
        "THEN" -> {keyword, then};
        "ELSE" -> {keyword, cobol_else}; % Éviter le mot-clé réservé 'else'
        "END-IF" -> {keyword, end_if};
        "PERFORM" -> {keyword, perform};
        "TIMES" -> {keyword, times};
        "PIC" -> {keyword, pic};
        "PICTURE" -> {keyword, pic};
        "VALUE" -> {keyword, value};
        "OCCURS" -> {keyword, occurs};
        "STOP" -> {keyword, stop};
        "RUN" -> {keyword, run};
        _ -> 
            case is_number_token(Token) of
                true -> {number, list_to_integer(Token)};
                false -> 
                    case is_string_literal_token(Token) of
                        true -> {string, Token};
                        false -> {identifier, list_to_atom(Token)}
                    end
            end
    end.

is_number_token(Token) ->
    try 
        _ = list_to_integer(Token),
        true
    catch
        _:_ -> false
    end.

is_string_literal_token([$" | _]) -> true;
is_string_literal_token([$' | _]) -> true;
is_string_literal_token(_) -> false.

%% Parser principal
parse_tokens(Tokens) ->
    try
        {Program, _Remaining} = parse_program(Tokens),
        {ok, Program}
    catch
        throw:Error -> {error, Error};
        _:Error -> {error, {parse_error, Error}}
    end.

parse_program(Tokens) ->
    {Identification, Rest1} = parse_identification_division(Tokens),
    {Environment, Rest2} = parse_environment_division(Rest1),
    {DataDiv, Rest3} = parse_data_division(Rest2),
    {ProcDiv, Rest4} = parse_procedure_division(Rest3),
    
    Program = #cobol_program{
        identification = Identification,
        environment = Environment,
        data_division = DataDiv,
        procedure_division = ProcDiv
    },
    {Program, Rest4}.

%% Parse IDENTIFICATION DIVISION
parse_identification_division([{keyword, identification}, {keyword, division} | Rest]) ->
    {ProgramId, Rest1} = parse_program_id(Rest),
    {Author, Rest2} = parse_optional_author(Rest1),
    {DateWritten, Rest3} = parse_optional_date_written(Rest2),
    
    Identification = #cobol_identification{
        program_id = ProgramId,
        author = Author,
        date_written = DateWritten
    },
    {Identification, Rest3};
parse_identification_division(Tokens) ->
    throw({expected_identification_division, Tokens}).

parse_program_id([{keyword, program_id}, {identifier, Name} | Rest]) ->
    {Name, Rest};
parse_program_id(Tokens) ->
    throw({expected_program_id, Tokens}).

parse_optional_author([{keyword, author}, {identifier, Name} | Rest]) ->
    {Name, Rest};
parse_optional_author(Tokens) ->
    {undefined, Tokens}.

parse_optional_date_written([{keyword, date_written}, {identifier, Date} | Rest]) ->
    {Date, Rest};
parse_optional_date_written(Tokens) ->
    {undefined, Tokens}.

%% Parse ENVIRONMENT DIVISION (simplifié)
parse_environment_division([{keyword, environment}, {keyword, division} | Rest]) ->
    {Rest1, _} = skip_until_next_division(Rest),
    {undefined, Rest1};
parse_environment_division(Tokens) ->
    {undefined, Tokens}.

%% Parse DATA DIVISION
parse_data_division([{keyword, data}, {keyword, division} | Rest]) ->
    {WorkingStorage, Rest1} = parse_working_storage_section(Rest),
    {FileSection, Rest2} = parse_file_section(Rest1),
    
    DataDiv = #cobol_data_division{
        working_storage = WorkingStorage,
        file_section = FileSection
    },
    {DataDiv, Rest2};
parse_data_division(Tokens) ->
    {#cobol_data_division{}, Tokens}.

parse_working_storage_section([{keyword, working_storage}, {keyword, section} | Rest]) ->
    parse_variables(Rest, []);
parse_working_storage_section(Tokens) ->
    {[], Tokens}.

parse_file_section([{keyword, file}, {keyword, section} | Rest]) ->
    {Rest1, _} = skip_until_next_division(Rest),
    {[], Rest1};
parse_file_section(Tokens) ->
    {[], Tokens}.

parse_variables([{number, Level}, {identifier, Name} | Rest], Acc) ->
    {Variable, Rest1} = parse_variable_definition(Level, Name, Rest),
    parse_variables(Rest1, [Variable | Acc]);
parse_variables(Tokens, Acc) ->
    {lists:reverse(Acc), Tokens}.

parse_variable_definition(Level, Name, Tokens) ->
    {Type, Rest1} = parse_picture_clause(Tokens),
    {Value, Rest2} = parse_value_clause(Rest1),
    {Occurs, Rest3} = parse_occurs_clause(Rest2),
    
    Variable = #cobol_variable{
        level = Level,
        name = Name,
        type = Type,
        value = Value,
        occurs = Occurs
    },
    {Variable, Rest3}.

parse_picture_clause([{keyword, pic}, {identifier, PicString} | Rest]) ->
    {atom_to_list(PicString), Rest};
parse_picture_clause(Tokens) ->
    {undefined, Tokens}.

parse_value_clause([{keyword, value}, Value | Rest]) ->
    {Value, Rest};
parse_value_clause(Tokens) ->
    {undefined, Tokens}.

parse_occurs_clause([{keyword, occurs}, {number, Times} | Rest]) ->
    {Times, Rest};
parse_occurs_clause(Tokens) ->
    {undefined, Tokens}.

%% Parse PROCEDURE DIVISION
parse_procedure_division([{keyword, procedure}, {keyword, division} | Rest]) ->
    {Paragraphs, Rest1} = parse_paragraphs(Rest, []),
    ProcDiv = #cobol_procedure_division{paragraphs = Paragraphs},
    {ProcDiv, Rest1};
parse_procedure_division(Tokens) ->
    {#cobol_procedure_division{}, Tokens}.

parse_paragraphs([{identifier, ParName} | Rest], Acc) ->
    {Statements, Rest1} = parse_statements(Rest, []),
    Paragraph = #cobol_paragraph{name = ParName, statements = Statements},
    parse_paragraphs(Rest1, [Paragraph | Acc]);
parse_paragraphs(Tokens, Acc) ->
    {lists:reverse(Acc), Tokens}.

parse_statements([{keyword, move} | Rest], Acc) ->
    {Stmt, Rest1} = parse_move_statement(Rest),
    parse_statements(Rest1, [Stmt | Acc]);
parse_statements([{keyword, display} | Rest], Acc) ->
    {Stmt, Rest1} = parse_display_statement(Rest),
    parse_statements(Rest1, [Stmt | Acc]);
parse_statements([{keyword, accept} | Rest], Acc) ->
    {Stmt, Rest1} = parse_accept_statement(Rest),
    parse_statements(Rest1, [Stmt | Acc]);
parse_statements([{keyword, cobol_if} | Rest], Acc) ->
    {Stmt, Rest1} = parse_if_statement(Rest),
    parse_statements(Rest1, [Stmt | Acc]);
parse_statements([{keyword, perform} | Rest], Acc) ->
    {Stmt, Rest1} = parse_perform_statement(Rest),
    parse_statements(Rest1, [Stmt | Acc]);
parse_statements([{keyword, stop}, {keyword, run} | Rest], Acc) ->
    parse_statements(Rest, [stop_run | Acc]);
parse_statements(Tokens, Acc) ->
    {lists:reverse(Acc), Tokens}.

parse_move_statement([Source, {keyword, to}, Target | Rest]) ->
    Stmt = {move_stmt, Source, Target},
    {Stmt, Rest}.

parse_display_statement([Item | Rest]) ->
    Stmt = {display_stmt, [Item]},
    {Stmt, Rest}.

parse_accept_statement([Target | Rest]) ->
    Stmt = {accept_stmt, Target},
    {Stmt, Rest}.

parse_if_statement(Tokens) ->
    {Condition, Rest1} = parse_condition(Tokens),
    {ThenClause, Rest2} = parse_then_clause(Rest1),
    {ElseClause, Rest3} = parse_else_clause(Rest2),
    Stmt = {if_stmt, Condition, ThenClause, ElseClause},
    {Stmt, Rest3}.

parse_perform_statement([Target | Rest]) ->
    case Rest of
        [{number, Times}, {keyword, times} | Rest1] ->
            Stmt = {perform_stmt, Target, Times},
            {Stmt, Rest1};
        _ ->
            Stmt = {perform_stmt, Target, undefined},
            {Stmt, Rest}
    end.

%% Fonctions utilitaires
parse_condition([Left, Op, Right | Rest]) ->
    {{condition, Left, Op, Right}, Rest};
parse_condition([Left | Rest]) ->
    {{condition, Left, {identifier, '='}, {identifier, 'TRUE'}}, Rest}.

parse_then_clause([{keyword, then} | Rest]) ->
    parse_clause_statements(Rest, []);
parse_then_clause(Rest) ->
    parse_clause_statements(Rest, []).

parse_else_clause([{keyword, cobol_else} | Rest]) ->
    {Statements, Rest1} = parse_clause_statements(Rest, []),
    {Statements, skip_end_if(Rest1)};
parse_else_clause(Rest) ->
    {[], skip_end_if(Rest)}.

parse_clause_statements([{keyword, end_if} | Rest], Acc) ->
    {lists:reverse(Acc), [{keyword, end_if} | Rest]};
parse_clause_statements([{keyword, cobol_else} | Rest], Acc) ->
    {lists:reverse(Acc), [{keyword, cobol_else} | Rest]};
parse_clause_statements([{keyword, move} | Rest], Acc) ->
    {Stmt, Rest1} = parse_move_statement(Rest),
    parse_clause_statements(Rest1, [Stmt | Acc]);
parse_clause_statements([{keyword, display} | Rest], Acc) ->
    {Stmt, Rest1} = parse_display_statement(Rest),
    parse_clause_statements(Rest1, [Stmt | Acc]);
parse_clause_statements(Tokens, Acc) ->
    {lists:reverse(Acc), Tokens}.

skip_end_if([{keyword, end_if} | Rest]) -> Rest;
skip_end_if([_ | Rest]) -> skip_end_if(Rest);
skip_end_if([]) -> [].

skip_until_next_division(Tokens) ->
    skip_until_division(Tokens, []).

skip_until_division([{keyword, _}, {keyword, division} | _] = Tokens, Acc) ->
    {Tokens, lists:reverse(Acc)};
skip_until_division([Token | Rest], Acc) ->
    skip_until_division(Rest, [Token | Acc]);
skip_until_division([], Acc) ->
    {[], lists:reverse(Acc)}.
