(* TODO create recursive descent parser *)

(* The RDP is comprised of a set of rules. Each rule r takes in a parse tree and 
   a sequence of tokens *)


(*

START -> TU 
TU -> ED | TU ED 
ED -> FN_DEF | DECL 
FN_DEF -> DECL_SPEC DECL DECL_LIST CS | DECL_SPEC DECL CS

DECL -> PTR DIR_DECL | DIR_DECL

DIR_DECL -> 

DECL_LIST -> DECL | DECL_LIST DECL

DECL_SPEC ->   SC_SPEC DECL_SPEC | SC_SPEC 
             | T_SPEC DECL_SPEC | T_SPEC 
             | T_QUAL DECL_SPEC | T_QUAL 
             | FN_SPEC DECL_SPEC | FN_SPEC 
             | ALIGN_SPEC DECL_SPEC | ALIGN_SPEC

Storage classes: https://en.cppreference.com/w/c/language/storage_duration
SC_SPEC -> typedef | extern | static | _Thread_local | auto | register

Types:
T_SPEC  -> void | char | short | int | long | float | double | signed | unsigned | bool | _Complex | _Imaginary | ...

Functions:
FN_SPEC -> inline | _Noreturn



DECLR -> DECL_SPEC ; | DECL_SPEC DECL_LIST ; | STATIC_ASSERT_DECL
DECL_LIST -> INIT_DECL | DECL_LIST , INIT_DECL
INIT_DECL -> DECL = INIT | DECL 

// TODO INIT

CS -> {} | { CL }
CL -> ITEM | CL ITEM

ITEM -> DECLR | S 
S -> LS | CS | ES | SS | IS | JS

LS -> ID : S | case CONSTEXPR : S | default : S 
ES -> ; | EXPR ;
SS -> if ( EXPR ) S else S | if ( EXPR ) S | SWITCH ( expr ) S 
IS -> while ( EXPR ) S | do S while ( EXPR ) ; 
      | for ( ES ES ) S | for (ES ES E) S | for ( DECLR ES ) S | for ( DECLR ES E ) S 
JS -> goto ID ; | continue ; | break ; | return ; | return EXPR ;

*)
