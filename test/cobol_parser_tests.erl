-module(cobol_parser_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    Path = "../../../aleph/target/release/alephc.exe",
    ale_call:set_rust_generator_path(Path),
    ok.

cobol_source() ->
    "IDENTIFICATION DIVISION.\n" ++
    "PROGRAM-ID. TEST.\n" ++
    "DATA DIVISION.\n" ++
    "WORKING-STORAGE SECTION.\n" ++
    "01 VAR1 PIC 9(1) VALUE 3.\n" ++
    "01 VAR2 PIC 9(1) VALUE 4.\n" ++
    "PROCEDURE DIVISION.\n" ++
    "STOP RUN.\n".

cobol_3_plus_4_generate_test() ->
    setup(),

    Cobol = cobol_source(),

    {ok, AlephAST} = cobol_parser:parse(Cobol),
    ?assert(is_map(AlephAST)),

    {ok, GeneratedCode} = ale_call:generate(AlephAST),

    _GeneratedString = binary_to_list(GeneratedCode),
    ok.

