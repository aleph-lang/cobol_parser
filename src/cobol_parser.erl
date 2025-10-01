%% Module parser COBOL compatible avec AlephTree étendu (Rust)
-module(cobol_parser).
-export([parse/1, parse_file/1, tokenize/1]).

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

%% Conversion vers AlephTree étendu
cobol_to_aleph(#cobol_program{
    identification = Id,
    environment = Env,
    data_division = DataDiv,
    procedure_division = ProcDiv
}) ->
    ProgramIdStr = case Id of
        #cobol_identification{program_id = PId} when PId =/= undefined ->
            atom_to_list(PId);
        _ -> "UNKNOWN"
    end,

    EnvironmentDiv = case Env of
        undefined -> null;
        _ -> aleph_environment_division(null, null)
    end,

    DataDivNode = case DataDiv of
        #cobol_data_division{working_storage = WS, file_section = FS} ->
            WSSection = case WS of
                [] -> null;
                Variables -> aleph_working_storage_section(Variables)
            end,
            FSSection = case FS of
                [] -> null;
                Files -> aleph_file_section(Files)
            end,
            aleph_data_division(FSSection, WSSection, null);
        _ -> null
    end,

    ProcDivNode = case ProcDiv of
        #cobol_procedure_division{paragraphs = Pars} ->
            Statements = lists:flatten([convert_paragraph_to_statements(Par) || Par <- Pars]),
            aleph_procedure_division(null, Statements);
        _ -> aleph_procedure_division(null, [])
    end,

    aleph_cobol_program(ProgramIdStr, EnvironmentDiv, DataDivNode, ProcDivNode).

convert_paragraph_to_statements(#cobol_paragraph{name = Name, statements = Stmts}) ->
    ParNode = aleph_paragraph(atom_to_list(Name), [convert_statement_to_aleph(S) || S <- Stmts]),
    [ParNode].

aleph_working_storage_section(Variables) ->
    VarNodes = [convert_variable_to_pic_clause(Var) || Var <- Variables],
    #{<<"type">> => <<"WorkingStorageSection">>, <<"variables">> => VarNodes}.

aleph_file_section(Files) ->
    #{<<"type">> => <<"FileSection">>, <<"files">> => Files}.

convert_variable_to_pic_clause(#cobol_variable{
    level = Level,
    name = Name,
    type = Type,
    value = Value,
    occurs = Occurs
}) ->
    VarName = atom_to_list(Name),
    LevelStr = integer_to_list(Level),
    PictureStr = case Type of
        undefined -> "X";
        TypeStr -> TypeStr
    end,
    
    InitValue = case Value of
        undefined -> null;
        {string, StrVal} -> aleph_string(StrVal);
        {number, NumVal} -> aleph_int(integer_to_list(NumVal));
        _ -> null
    end,

    OccursStr = case Occurs of
        undefined -> null;
        N -> integer_to_list(N)
    end,

    aleph_pic_clause(VarName, LevelStr, PictureStr, InitValue, OccursStr, null).

convert_statement_to_aleph({move_stmt, Source, Target}) ->
    SourceNode = convert_expr_to_aleph(Source),
    TargetNodes = [convert_expr_to_aleph(Target)],
    aleph_move(SourceNode, TargetNodes);

convert_statement_to_aleph({display_stmt, Items}) ->
    ItemNodes = [convert_expr_to_aleph(Item) || Item <- Items],
    aleph_display(ItemNodes, null);

convert_statement_to_aleph({accept_stmt, Target}) ->
    TargetNode = convert_expr_to_aleph(Target),
    aleph_accept(TargetNode, null);

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

    TimesClause = case Times of
        undefined -> null;
        N when is_integer(N) -> aleph_int(integer_to_list(N))
    end,

    aleph_perform(TargetName, null, null, TimesClause, null, null, null);

convert_statement_to_aleph({evaluate_stmt, Subject, WhenClauses, WhenOther}) ->
    SubjectNode = convert_expr_to_aleph(Subject),
    WhenNodes = [convert_when_clause(W) || W <- WhenClauses],
    OtherNode = case WhenOther of
        undefined -> null;
        Stmts -> 
            ConvertedStmts = [convert_statement_to_aleph(S) || S <- Stmts],
            lists:foldl(fun(Node, Acc) -> aleph_stmts(Acc, Node) end,
                       hd(ConvertedStmts), tl(ConvertedStmts))
    end,
    aleph_evaluate(SubjectNode, WhenNodes, OtherNode);

convert_statement_to_aleph({call_stmt, ProgramName, UsingParams}) ->
    ProgramNode = convert_expr_to_aleph(ProgramName),
    ParamNodes = case UsingParams of
        undefined -> null;
        Params -> [convert_expr_to_aleph(P) || P <- Params]
    end,
    aleph_call(ProgramNode, ParamNodes, null, null);

convert_statement_to_aleph({string_stmt, SourceItems, DelimitedBy, IntoItem}) ->
    SourceNodes = [convert_expr_to_aleph(S) || S <- SourceItems],
    DelimNode = convert_expr_to_aleph(DelimitedBy),
    IntoNode = convert_expr_to_aleph(IntoItem),
    aleph_string_stmt(SourceNodes, DelimNode, IntoNode, null, null);

convert_statement_to_aleph({inspect_stmt, InspectingItem, TallyingClause, ReplacingClause}) ->
    InspectingNode = convert_expr_to_aleph(InspectingItem),
    TallyingNode = case TallyingClause of
        undefined -> null;
        TallyingValue -> convert_expr_to_aleph(TallyingValue)
    end,
    ReplacingNode = case ReplacingClause of
        undefined -> null;
        ReplacingValue -> convert_expr_to_aleph(ReplacingValue)
    end,
    aleph_inspect(InspectingNode, TallyingNode, ReplacingNode);

convert_statement_to_aleph({compute_stmt, Target, Expression}) ->
    TargetNode = convert_expr_to_aleph(Target),
    ExprNode = convert_expr_to_aleph(Expression),
    aleph_compute(TargetNode, ExprNode, null);

convert_statement_to_aleph({open_stmt, Mode, FileList}) ->
    ModeStr = atom_to_list(Mode),
    FileNames = [atom_to_list(F) || F <- FileList],
    aleph_open(ModeStr, FileNames);

convert_statement_to_aleph({close_stmt, FileList}) ->
    FileNames = [atom_to_list(F) || F <- FileList],
    aleph_close(FileNames);

convert_statement_to_aleph({write_stmt, RecordName, FromClause}) ->
    RecordNameStr = atom_to_list(RecordName),
    FromNode = case FromClause of
        undefined -> null;
        TallyingValue -> convert_expr_to_aleph(TallyingValue)
    end,
    aleph_write(RecordNameStr, FromNode, null);

convert_statement_to_aleph({goto_stmt, Target}) ->
    TargetStr = atom_to_list(Target),
    aleph_goto(TargetStr, null);

convert_statement_to_aleph(stop_run) ->
    aleph_stop("RUN");

convert_statement_to_aleph(exit_stmt) ->
    aleph_exit();

convert_statement_to_aleph(Stmt) ->
    aleph_comment("UNKNOWN STATEMENT: " ++ io_lib:format("~p", [Stmt])).

convert_when_clause({when_clause, SelectionObject, Statements}) ->
    ObjectNode = convert_expr_to_aleph(SelectionObject),
    StmtNodes = [convert_statement_to_aleph(S) || S <- Statements],
    aleph_when_clause(ObjectNode, StmtNodes).

convert_expr_to_aleph({identifier, Name}) ->
    aleph_var(atom_to_list(Name), "false");
convert_expr_to_aleph({qualified_name, DataName, Qualifiers}) ->
    DataNameStr = atom_to_list(DataName),
    QualifierStrs = [atom_to_list(Q) || Q <- Qualifiers],
    aleph_qualified_name(DataNameStr, QualifierStrs);
convert_expr_to_aleph({subscript, DataName, Subscripts}) ->
    DataNameStr = atom_to_list(DataName),
    SubscriptNodes = [convert_expr_to_aleph(S) || S <- Subscripts],
    aleph_subscript(DataNameStr, SubscriptNodes);
convert_expr_to_aleph({string, Value}) ->
    aleph_string(Value);
convert_expr_to_aleph({number, Value}) ->
    aleph_int(integer_to_list(Value));
convert_expr_to_aleph({figurative, Type}) ->
    TypeStr = atom_to_list(Type),
    aleph_figurative(TypeStr);
convert_expr_to_aleph({hex_literal, Value}) ->
    aleph_hex_literal(Value);
convert_expr_to_aleph({class_condition, DataItem, ClassName}) ->
    DataNode = convert_expr_to_aleph(DataItem),
    ClassStr = atom_to_list(ClassName),
    aleph_class_condition(DataNode, ClassStr);
convert_expr_to_aleph({sign_condition, DataItem, Sign}) ->
    DataNode = convert_expr_to_aleph(DataItem),
    SignStr = atom_to_list(Sign),
    aleph_sign_condition(DataNode, SignStr);
convert_expr_to_aleph({condition, Left, Op, Right}) ->
    LeftNode = convert_expr_to_aleph(Left),
    RightNode = convert_expr_to_aleph(Right),
    case Op of
        {identifier, "="} -> aleph_eq(LeftNode, RightNode);
        {identifier, "<"} -> aleph_le(LeftNode, RightNode);
        {identifier, ">"} -> aleph_le(RightNode, LeftNode);
        _ -> aleph_eq(LeftNode, RightNode)
    end;
convert_expr_to_aleph({arithmetic_expr, Left, Op, Right}) ->
    LeftNode = convert_expr_to_aleph(Left),
    RightNode = convert_expr_to_aleph(Right),
    case Op of
        {identifier, "+"} -> aleph_add(LeftNode, RightNode);
        {identifier, "-"} -> aleph_sub(LeftNode, RightNode);
        {identifier, "*"} -> aleph_mul(LeftNode, RightNode);
        {identifier, "/"} -> aleph_div(LeftNode, RightNode);
        _ -> aleph_add(LeftNode, RightNode)
    end;
convert_expr_to_aleph(Expr) when is_atom(Expr) ->
    aleph_var(atom_to_list(Expr), "false");
convert_expr_to_aleph(Expr) when is_list(Expr) ->
    aleph_string(Expr);
convert_expr_to_aleph(Expr) when is_integer(Expr) ->
    aleph_int(integer_to_list(Expr));
convert_expr_to_aleph(Expr) ->
    aleph_comment("UNKNOWN EXPR: " ++ io_lib:format("~p", [Expr])).

%% Fonctions de construction des nouveaux nœuds AlephTree COBOL
aleph_cobol_program(ProgramId, EnvironmentDiv, DataDiv, ProcedureDiv) ->
    #{<<"type">> => <<"CobolProgram">>,
      <<"programId">> => list_to_binary(ProgramId),
      <<"environmentDiv">> => EnvironmentDiv,
      <<"dataDiv">> => DataDiv,
      <<"procedureDiv">> => ProcedureDiv}.

aleph_environment_division(ConfigSection, IOControlSection) ->
    #{<<"type">> => <<"EnvironmentDivision">>,
      <<"configSection">> => ConfigSection,
      <<"ioControlSection">> => IOControlSection}.

aleph_data_division(FileSection, WorkingStorageSection, LinkageSection) ->
    #{<<"type">> => <<"DataDivision">>,
      <<"fileSection">> => FileSection,
      <<"workingStorageSection">> => WorkingStorageSection,
      <<"linkageSection">> => LinkageSection}.

aleph_procedure_division(UsingClause, Statements) ->
    #{<<"type">> => <<"ProcedureDivision">>,
      <<"usingClause">> => UsingClause,
      <<"statements">> => Statements}.

aleph_pic_clause(DataName, LevelNumber, Picture, InitialValue, OccursClause, Usage) ->
    #{<<"type">> => <<"PicClause">>,
      <<"dataName">> => list_to_binary(DataName),
      <<"levelNumber">> => list_to_binary(LevelNumber),
      <<"picture">> => list_to_binary(Picture),
      <<"initialValue">> => InitialValue,
      <<"occursClause">> => case OccursClause of
                              null -> null;
                              Str -> list_to_binary(Str)
                          end,
      <<"usage">> => Usage}.

aleph_move(Source, TargetList) ->
    #{<<"type">> => <<"Move">>,
      <<"source">> => Source,
      <<"targetList">> => TargetList}.

aleph_compute(Target, Expression, OnSizeError) ->
    #{<<"type">> => <<"Compute">>,
      <<"target">> => Target,
      <<"expression">> => Expression,
      <<"onSizeError">> => OnSizeError}.

aleph_perform(TargetParagraph, FromParagraph, ThroughParagraph, TimesClause, UntilClause, VaryingClause, InlineStatements) ->
    #{<<"type">> => <<"Perform">>,
      <<"targetParagraph">> => case TargetParagraph of
                                 null -> null;
                                 Str -> list_to_binary(Str)
                               end,
      <<"fromParagraph">> => FromParagraph,
      <<"throughParagraph">> => ThroughParagraph,
      <<"timesClause">> => TimesClause,
      <<"untilClause">> => UntilClause,
      <<"varyingClause">> => VaryingClause,
      <<"inlineStatements">> => InlineStatements}.

aleph_accept(Target, FromDevice) ->
    #{<<"type">> => <<"Accept">>,
      <<"target">> => Target,
      <<"fromDevice">> => FromDevice}.

aleph_display(ItemList, UponDevice) ->
    #{<<"type">> => <<"Display">>,
      <<"itemList">> => ItemList,
      <<"uponDevice">> => UponDevice}.

aleph_open(Mode, FileList) ->
    #{<<"type">> => <<"Open">>,
      <<"mode">> => list_to_binary(Mode),
      <<"fileList">> => [list_to_binary(F) || F <- FileList]}.

aleph_close(FileList) ->
    #{<<"type">> => <<"Close">>,
      <<"fileList">> => [list_to_binary(F) || F <- FileList]}.

aleph_write(RecordName, FromClause, AdvancingClause) ->
    #{<<"type">> => <<"Write">>,
      <<"recordName">> => list_to_binary(RecordName),
      <<"fromClause">> => FromClause,
      <<"advancingClause">> => AdvancingClause}.

aleph_goto(TargetParagraph, DependingOn) ->
    #{<<"type">> => <<"GoTo">>,
      <<"targetParagraph">> => list_to_binary(TargetParagraph),
      <<"dependingOn">> => DependingOn}.

aleph_stop(StopType) ->
    #{<<"type">> => <<"Stop">>,
      <<"stopType">> => list_to_binary(StopType)}.

aleph_exit() ->
    #{<<"type">> => <<"Exit">>}.

aleph_paragraph(Name, Statements) ->
    #{<<"type">> => <<"Paragraph">>,
      <<"name">> => list_to_binary(Name),
      <<"statements">> => Statements}.

aleph_evaluate(SelectionSubject, WhenClauses, WhenOther) ->
    #{<<"type">> => <<"Evaluate">>,
      <<"selectionSubject">> => SelectionSubject,
      <<"whenClauses">> => WhenClauses,
      <<"whenOther">> => WhenOther}.

aleph_when_clause(SelectionObject, Statements) ->
    #{<<"type">> => <<"WhenClause">>,
      <<"selectionObject">> => SelectionObject,
      <<"statements">> => Statements}.

aleph_inspect(InspectingItem, TallyingClause, ReplacingClause) ->
    #{<<"type">> => <<"Inspect">>,
      <<"inspectingItem">> => InspectingItem,
      <<"tallyingClause">> => TallyingClause,
      <<"replacingClause">> => ReplacingClause}.

aleph_string_stmt(SourceItems, DelimitedBy, IntoItem, WithPointer, OnOverflow) ->
    #{<<"type">> => <<"StringStmt">>,
      <<"sourceItems">> => SourceItems,
      <<"delimitedBy">> => DelimitedBy,
      <<"intoItem">> => IntoItem,
      <<"withPointer">> => WithPointer,
      <<"onOverflow">> => OnOverflow}.

aleph_call(ProgramName, UsingParameters, GivingParameter, OnException) ->
    #{<<"type">> => <<"Call">>,
      <<"programName">> => ProgramName,
      <<"usingParameters">> => UsingParameters,
      <<"givingParameter">> => GivingParameter,
      <<"onException">> => OnException}.

aleph_qualified_name(DataName, QualifierList) ->
    #{<<"type">> => <<"QualifiedName">>,
      <<"dataName">> => list_to_binary(DataName),
      <<"qualifierList">> => [list_to_binary(Q) || Q <- QualifierList]}.

aleph_subscript(DataName, SubscriptList) ->
    #{<<"type">> => <<"Subscript">>,
      <<"dataName">> => list_to_binary(DataName),
      <<"subscriptList">> => SubscriptList}.

aleph_figurative(FigurativeType) ->
    #{<<"type">> => <<"Figurative">>,
      <<"figurativeType">> => list_to_binary(FigurativeType)}.

aleph_class_condition(DataItem, ClassName) ->
    #{<<"type">> => <<"ClassCondition">>,
      <<"dataItem">> => DataItem,
      <<"className">> => list_to_binary(ClassName)}.

aleph_sign_condition(DataItem, Sign) ->
    #{<<"type">> => <<"SignCondition">>,
      <<"dataItem">> => DataItem,
      <<"sign">> => list_to_binary(Sign)}.

aleph_hex_literal(Value) ->
    #{<<"type">> => <<"HexLiteral">>,
      <<"value">> => list_to_binary(Value)}.

aleph_unit() ->
    #{<<"type">> => <<"Unit">>}.

aleph_int(Value) ->
    #{<<"type">> => <<"Int">>, <<"value">> => list_to_binary(Value)}.

aleph_string(Value) ->
    #{<<"type">> => <<"String">>, <<"value">> => list_to_binary(Value)}.

aleph_var(Var, IsPointer) ->
    #{<<"type">> => <<"Var">>,
      <<"var">> => list_to_binary(Var),
      <<"isPointer">> => list_to_binary(IsPointer)}.

aleph_if(Condition, Then, Else) ->
    #{<<"type">> => <<"If">>,
      <<"condition">> => Condition,
      <<"then">> => Then,
      <<"else">> => Else}.

aleph_add(Expr1, Expr2) ->
    #{<<"type">> => <<"Add">>,
      <<"numberExpr1">> => Expr1,
      <<"numberExpr2">> => Expr2}.

aleph_sub(Expr1, Expr2) ->
    #{<<"type">> => <<"Sub">>,
      <<"numberExpr1">> => Expr1,
      <<"numberExpr2">> => Expr2}.

aleph_mul(Expr1, Expr2) ->
    #{<<"type">> => <<"Mul">>,
      <<"numberExpr1">> => Expr1,
      <<"numberExpr2">> => Expr2}.

aleph_div(Expr1, Expr2) ->
    #{<<"type">> => <<"Div">>,
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

aleph_stmts(Expr1, Expr2) ->
    #{<<"type">> => <<"Stmts">>,
      <<"expr1">> => Expr1,
      <<"expr2">> => Expr2}.

aleph_comment(Value) ->
    #{<<"type">> => <<"Comment">>,
      <<"value">> => list_to_binary(Value)}.

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
        "IF" -> {keyword, cobol_if};
        "THEN" -> {keyword, then};
        "ELSE" -> {keyword, cobol_else};
        "END-IF" -> {keyword, end_if};
        "PERFORM" -> {keyword, perform};
        "TIMES" -> {keyword, times};
        "PIC" -> {keyword, pic};
        "PICTURE" -> {keyword, pic};
        "VALUE" -> {keyword, value};
        "OCCURS" -> {keyword, occurs};
        "STOP" -> {keyword, stop};
        "RUN" -> {keyword, run};
        "EXIT" -> {keyword, exit};
        "EVALUATE" -> {keyword, evaluate};
        "WHEN" -> {keyword, cobol_when};
        "OTHER" -> {keyword, other};
        "END-EVALUATE" -> {keyword, end_evaluate};
        "CALL" -> {keyword, call};
        "USING" -> {keyword, using};
        "GIVING" -> {keyword, giving};
        "ON" -> {keyword, on};
        "EXCEPTION" -> {keyword, exception};
        "STRING" -> {keyword, string};
        "DELIMITED" -> {keyword, delimited};
        "BY" -> {keyword, by};
        "INTO" -> {keyword, into};
        "WITH" -> {keyword, with};
        "POINTER" -> {keyword, pointer};
        "OVERFLOW" -> {keyword, overflow};
        "UNSTRING" -> {keyword, unstring};
        "INSPECT" -> {keyword, inspect};
        "TALLYING" -> {keyword, tallying};
        "REPLACING" -> {keyword, replacing};
        "COMPUTE" -> {keyword, compute};
        "SIZE" -> {keyword, size};
        "ERROR" -> {keyword, error};
        "OPEN" -> {keyword, open};
        "INPUT" -> {keyword, input};
        "OUTPUT" -> {keyword, output};
        "I-O" -> {keyword, io};
        "EXTEND" -> {keyword, extend};
        "CLOSE" -> {keyword, close};
        "READ" -> {keyword, read};
        "WRITE" -> {keyword, write};
        "AT" -> {keyword, at};
        "END" -> {keyword, cobol_end};
        "NOT" -> {keyword, cobol_not};
        "FROM" -> {keyword, from};
        "ADVANCING" -> {keyword, advancing};
        "GO" -> {keyword, go};
        "DEPENDING" -> {keyword, depending};
        "ZERO" -> {keyword, zero};
        "ZEROS" -> {keyword, zero};
        "ZEROES" -> {keyword, zero};
        "SPACE" -> {keyword, space};
        "SPACES" -> {keyword, space};
        "HIGH-VALUE" -> {keyword, high_value};
        "HIGH-VALUES" -> {keyword, high_value};
        "LOW-VALUE" -> {keyword, low_value};
        "LOW-VALUES" -> {keyword, low_value};
        "QUOTE" -> {keyword, quote};
        "QUOTES" -> {keyword, quote};
        "ALL" -> {keyword, all};
        "NUMERIC" -> {keyword, numeric};
        "ALPHABETIC" -> {keyword, alphabetic};
        "ALPHABETIC-UPPER" -> {keyword, alphabetic_upper};
        "ALPHABETIC-LOWER" -> {keyword, alphabetic_lower};
        "POSITIVE" -> {keyword, positive};
        "NEGATIVE" -> {keyword, negative};
        "COMP" -> {keyword, comp};
        "COMP-1" -> {keyword, comp_1};
        "COMP-2" -> {keyword, comp_2};
        "COMP-3" -> {keyword, comp_3};
        "BINARY" -> {keyword, binary};
        "PACKED-DECIMAL" -> {keyword, packed_decimal};
        "USAGE" -> {keyword, usage};
        "IS" -> {keyword, is};
        "INDEXED" -> {keyword, indexed};
        "VARYING" -> {keyword, varying};
        "UNTIL" -> {keyword, until};
        "THROUGH" -> {keyword, through};
        "THRU" -> {keyword, through};
        _ ->
            case is_number_token(Token) of
                true -> {number, list_to_integer(Token)};
                false ->
                    case is_string_literal_token(Token) of
                        true -> {string, Token};
                        false -> 
                            case is_hex_literal_token(Token) of
                                true -> {hex_literal, Token};
                                false -> {identifier, list_to_atom(Token)}
                            end
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

is_hex_literal_token([$X, $" | _]) -> true;
is_hex_literal_token([$X, $' | _]) -> true;
is_hex_literal_token(_) -> false.

%% Parser principal (étendu)
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

%% Parse DATA DIVISION (étendu)
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

%% Parse PROCEDURE DIVISION (étendu)
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

%% Parse des instructions étendues
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
parse_statements([{keyword, evaluate} | Rest], Acc) ->
    {Stmt, Rest1} = parse_evaluate_statement(Rest),
    parse_statements(Rest1, [Stmt | Acc]);
parse_statements([{keyword, call} | Rest], Acc) ->
    {Stmt, Rest1} = parse_call_statement(Rest),
    parse_statements(Rest1, [Stmt | Acc]);
parse_statements([{keyword, string} | Rest], Acc) ->
    {Stmt, Rest1} = parse_string_statement(Rest),
    parse_statements(Rest1, [Stmt | Acc]);
parse_statements([{keyword, unstring} | Rest], Acc) ->
    {Stmt, Rest1} = parse_unstring_statement(Rest),
    parse_statements(Rest1, [Stmt | Acc]);
parse_statements([{keyword, inspect} | Rest], Acc) ->
    {Stmt, Rest1} = parse_inspect_statement(Rest),
    parse_statements(Rest1, [Stmt | Acc]);
parse_statements([{keyword, compute} | Rest], Acc) ->
    {Stmt, Rest1} = parse_compute_statement(Rest),
    parse_statements(Rest1, [Stmt | Acc]);
parse_statements([{keyword, open} | Rest], Acc) ->
    {Stmt, Rest1} = parse_open_statement(Rest),
    parse_statements(Rest1, [Stmt | Acc]);
parse_statements([{keyword, close} | Rest], Acc) ->
    {Stmt, Rest1} = parse_close_statement(Rest),
    parse_statements(Rest1, [Stmt | Acc]);
parse_statements([{keyword, read} | Rest], Acc) ->
    {Stmt, Rest1} = parse_read_statement(Rest),
    parse_statements(Rest1, [Stmt | Acc]);
parse_statements([{keyword, write} | Rest], Acc) ->
    {Stmt, Rest1} = parse_write_statement(Rest),
    parse_statements(Rest1, [Stmt | Acc]);
parse_statements([{keyword, go}, {keyword, to} | Rest], Acc) ->
    {Stmt, Rest1} = parse_goto_statement(Rest),
    parse_statements(Rest1, [Stmt | Acc]);
parse_statements([{keyword, stop}, {keyword, run} | Rest], Acc) ->
    parse_statements(Rest, [stop_run | Acc]);
parse_statements([{keyword, exit} | Rest], Acc) ->
    parse_statements(Rest, [exit_stmt | Acc]);
parse_statements(Tokens, Acc) ->
    {lists:reverse(Acc), Tokens}.

%% Parsers pour les nouvelles instructions
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

parse_evaluate_statement([Subject | Rest]) ->
    {WhenClauses, Rest1} = parse_when_clauses(Rest, []),
    {WhenOther, Rest2} = parse_when_other_clause(Rest1),
    Rest3 = skip_end_evaluate(Rest2),
    Stmt = {evaluate_stmt, Subject, WhenClauses, WhenOther},
    {Stmt, Rest3}.

parse_when_clauses([{keyword, cobol_when}, Object | Rest], Acc) ->
    {Statements, Rest1} = parse_when_statements(Rest, []),
    WhenClause = {when_clause, Object, Statements},
    parse_when_clauses(Rest1, [WhenClause | Acc]);
parse_when_clauses(Tokens, Acc) ->
    {lists:reverse(Acc), Tokens}.

parse_when_other_clause([{keyword, cobol_when}, {keyword, other} | Rest]) ->
    {Statements, Rest1} = parse_when_statements(Rest, []),
    {Statements, Rest1};
parse_when_other_clause(Tokens) ->
    {undefined, Tokens}.

parse_when_statements([{keyword, cobol_when} | _] = Tokens, Acc) ->
    {lists:reverse(Acc), Tokens};
parse_when_statements([{keyword, when_other} | _] = Tokens, Acc) ->
    {lists:reverse(Acc), Tokens};
parse_when_statements([{keyword, end_evaluate} | _] = Tokens, Acc) ->
    {lists:reverse(Acc), Tokens};
parse_when_statements([{keyword, move} | Rest], Acc) ->
    {Stmt, Rest1} = parse_move_statement(Rest),
    parse_when_statements(Rest1, [Stmt | Acc]);
parse_when_statements([{keyword, display} | Rest], Acc) ->
    {Stmt, Rest1} = parse_display_statement(Rest),
    parse_when_statements(Rest1, [Stmt | Acc]);
parse_when_statements([_ | Rest], Acc) ->
    parse_when_statements(Rest, Acc);
parse_when_statements([], Acc) ->
    {lists:reverse(Acc), []}.

parse_call_statement([ProgramName | Rest]) ->
    {UsingParams, Rest1} = parse_using_clause(Rest),
    Stmt = {call_stmt, ProgramName, UsingParams},
    {Stmt, Rest1}.

parse_using_clause([{keyword, using} | Rest]) ->
    parse_parameter_list(Rest, []);
parse_using_clause(Tokens) ->
    {undefined, Tokens}.

parse_parameter_list([{identifier, Param} | Rest], Acc) ->
    parse_parameter_list(Rest, [Param | Acc]);
parse_parameter_list(Tokens, Acc) ->
    {lists:reverse(Acc), Tokens}.

parse_string_statement(Tokens) ->
    {SourceItems, Rest1} = parse_source_items(Tokens, []),
    {DelimitedBy, Rest2} = parse_delimited_by_clause(Rest1),
    {IntoItem, Rest3} = parse_into_clause(Rest2),
    Stmt = {string_stmt, SourceItems, DelimitedBy, IntoItem},
    {Stmt, Rest3}.

parse_source_items([{keyword, delimited} | _] = Tokens, Acc) ->
    {lists:reverse(Acc), Tokens};
parse_source_items([Item | Rest], Acc) ->
    parse_source_items(Rest, [Item | Acc]);
parse_source_items([], Acc) ->
    {lists:reverse(Acc), []}.

parse_delimited_by_clause([{keyword, delimited}, {keyword, by}, Delimiter | Rest]) ->
    {Delimiter, Rest};
parse_delimited_by_clause(Tokens) ->
    {{string, "SPACE"}, Tokens}.

parse_into_clause([{keyword, into}, Item | Rest]) ->
    {Item, Rest};
parse_into_clause(Tokens) ->
    throw({expected_into_clause, Tokens}).

parse_unstring_statement([SourceItem, {keyword, delimited}, {keyword, by}, Delimiter, {keyword, into} | Rest]) ->
    {IntoItems, Rest1} = parse_into_items(Rest, []),
    Stmt = {unstring_stmt, SourceItem, Delimiter, IntoItems},
    {Stmt, Rest1}.

parse_into_items([{identifier, Item} | Rest], Acc) ->
    parse_into_items(Rest, [Item | Acc]);
parse_into_items(Tokens, Acc) ->
    {lists:reverse(Acc), Tokens}.

parse_inspect_statement([InspectingItem | Rest]) ->
    {TallyingClause, Rest1} = parse_tallying_clause(Rest),
    {ReplacingClause, Rest2} = parse_replacing_clause(Rest1),
    Stmt = {inspect_stmt, InspectingItem, TallyingClause, ReplacingClause},
    {Stmt, Rest2}.

parse_tallying_clause([{keyword, tallying}, Clause | Rest]) ->
    {Clause, Rest};
parse_tallying_clause(Tokens) ->
    {undefined, Tokens}.

parse_replacing_clause([{keyword, replacing}, Clause | Rest]) ->
    {Clause, Rest};
parse_replacing_clause(Tokens) ->
    {undefined, Tokens}.

parse_compute_statement([Target, {identifier, '='} | Rest]) ->
    {Expression, Rest1} = parse_arithmetic_expression(Rest),
    Stmt = {compute_stmt, Target, Expression},
    {Stmt, Rest1}.

parse_arithmetic_expression([Left, Op, Right | Rest]) when Op == {identifier, '+'}; 
                                                           Op == {identifier, '-'}; 
                                                           Op == {identifier, '*'}; 
                                                           Op == {identifier, '/'} ->
    {{arithmetic_expr, Left, Op, Right}, Rest};
parse_arithmetic_expression([Expr | Rest]) ->
    {Expr, Rest}.

parse_open_statement([{keyword, Mode}, File | Rest]) when Mode == input; Mode == output; Mode == io; Mode == extend ->
    {FileList, Rest1} = parse_file_list(Rest, [File]),
    Stmt = {open_stmt, Mode, FileList},
    {Stmt, Rest1}.

parse_file_list([{identifier, File} | Rest], Acc) ->
    parse_file_list(Rest, [File | Acc]);
parse_file_list(Tokens, Acc) ->
    {lists:reverse(Acc), Tokens}.

parse_close_statement(Tokens) ->
    {FileList, Rest} = parse_file_list(Tokens, []),
    Stmt = {close_stmt, FileList},
    {Stmt, Rest}.

parse_read_statement([{identifier, FileName} | Rest]) ->
    {IntoClause, Rest1} = parse_optional_into_clause(Rest),
    {AtEndClause, Rest2} = parse_at_end_clause(Rest1),
    Stmt = {read_stmt, FileName, IntoClause, AtEndClause},
    {Stmt, Rest2}.

parse_optional_into_clause([{keyword, into}, Item | Rest]) ->
    {Item, Rest};
parse_optional_into_clause(Tokens) ->
    {undefined, Tokens}.

parse_at_end_clause([{keyword, at}, {keyword, cobol_end} | Rest]) ->
    {Statements, Rest1} = parse_clause_statements(Rest, []),
    {Statements, Rest1};
parse_at_end_clause(Tokens) ->
    {undefined, Tokens}.

parse_write_statement([{identifier, RecordName} | Rest]) ->
    {FromClause, Rest1} = parse_from_clause(Rest),
    Stmt = {write_stmt, RecordName, FromClause},
    {Stmt, Rest1}.

parse_from_clause([{keyword, from}, Item | Rest]) ->
    {Item, Rest};
parse_from_clause(Tokens) ->
    {undefined, Tokens}.

parse_goto_statement([{identifier, Target} | Rest]) ->
    Stmt = {goto_stmt, Target},
    {Stmt, Rest}.

%% Fonctions utilitaires étendues
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

skip_end_evaluate([{keyword, end_evaluate} | Rest]) -> Rest;
skip_end_evaluate([_ | Rest]) -> skip_end_evaluate(Rest);
skip_end_evaluate([]) -> [].

skip_until_next_division(Tokens) ->
    skip_until_division(Tokens, []).

skip_until_division([{keyword, _}, {keyword, division} | _] = Tokens, Acc) ->
    {Tokens, lists:reverse(Acc)};
skip_until_division([Token | Rest], Acc) ->
    skip_until_division(Rest, [Token | Acc]);
skip_until_division([], Acc) ->
    {[], lists:reverse(Acc)}.
