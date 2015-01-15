(*
   Parser spec for Crisp
   Nik Sultana, Cambridge University Computer Lab, January 2015

   Target parser-generator: menhir 20140422
*)

(*Native value interpretations*)
%token <int> INTEGER (*FIXME is OCaml's "int" of the precision we want to support?*)
%token <string> STRING
%token <bool> BOOLEAN
(*FIXME include float?*)
(*FIXME need to include unit type + value*)

(*Punctuation*)
%token COLON
%token SEMICOLON
%token BANG
%token QUESTION
%token PERIOD
%token COLONCOLON
%token LEFT_R_BRACKET
%token RIGHT_R_BRACKET
%token LEFT_S_BRACKET
%token RIGHT_S_BRACKET
%token LEFT_C_BRACKET
%token RIGHT_C_BRACKET
%token LEFT_A_BRACKET
%token RIGHT_A_BRACKET
%token AT
%token PIPE
%token PLUS
%token UNDERSCORE
%token DASH
%token ASTERIX
%token SLASH
%token EOF
%token COMMA
%token NL
%token HASH

(*Since we're relying on the offside rule for scoping, code blocks aren't
  explicitly delimited as in e.g., Algol-68-style languages.*)
%token <int> UNDENTN
(*The lexer will produce UNDENTN tokens, then a filter (sitting between
  the lexer and parser) will expand these into UNDENT tokens.*)
%token INDENT
%token UNDENT
(* NOTE that INDENT also means that a NL occurred (before the indent)
        and UNDENT also means that a newline occurred (before the first indent).
        UNDENTN n means that NL followed by UNDENTs occurred.
*)

(*Reserved words*)
%token IF
%token ELSE
%token IN
%token DEF
%token CARRY_ON
%token YIELD
%token TYPE
%token TYPE_INTEGER
%token TYPE_BOOLEAN
%token TYPE_STRING
%token TYPE_RECORD
%token TYPE_VARIANT
%token CASE
%token OF
%token AND
%token NOT
%token OR
%token IMPORT

(*Names*)
%token <string> UPPER_ALPHA
%token <string> LOWER_ALPHA
%token <string> NAT_NUM
%token <string> VARIABLE
%token <string> IDENTIFIER

%start <Crisp_syntax.program> program
%%

program:
  | EOF {[]}
  | NL; p = program {p}
  | e = toplevel_decl; p = program {e :: p}

base_type:
  | TYPE_STRING {fun name -> Crisp_syntax.String name}
  | TYPE_INTEGER {fun name -> Crisp_syntax.Integer name}
  | TYPE_BOOLEAN {fun name -> Crisp_syntax.Boolean name}

(*FIXME need to include termination conditions for lists and string*)
(*FIXME include byte-order annotations*)
(*The kinds of type declarations we want to parse:

   type alias_example: string

   type record_example: record
     l1 : string
     l2 : integer

   type variant_example: variant
     l1 : string
     l2 : integer

   type compound_example: variant
     l1 : integer
     l2 : record
       l3 : string
       l4 : integer
     l5 : integer
*)

type_line:
  | value_name = IDENTIFIER; COLON; td = type_def {td (Some value_name)}

type_lines:
  | tl = type_line; NL; rest = type_lines { tl :: rest }
  | tl = type_line; UNDENT { [tl] }

type_def:
  | bt = base_type
    {fun (name : Crisp_syntax.label option) -> bt name}
  | TYPE_RECORD; INDENT; tl = type_lines
    {fun (name : Crisp_syntax.label option) -> Crisp_syntax.Record (name, List.rev tl)}
  | TYPE_VARIANT; INDENT; tl = type_lines
    {fun (name : Crisp_syntax.label option) -> Crisp_syntax.Disjoint_Union (name, List.rev tl)}

type_decl:
  | TYPE; type_name = IDENTIFIER; COLON; td = type_def
    { {Crisp_syntax.type_name = type_name;
       Crisp_syntax.type_value = td None} }

(*TODO include process definitions*)
toplevel_decl:
  | ty_decl = type_decl {Crisp_syntax.Type ty_decl}
