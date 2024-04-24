/* //////////////////////////////////////////////////////////////////////////////////////
//////该规则为用户规则文件，用于ANTLR解析ST编程语言，语法名为ST，program为主规则，所有规则皆可从该规则出发达到
//////POU（Program Organization Unit）是 IEC 61131-3 标准中的程序组织单元，为本规则的主要内容
//////包括程序体（Program）、函数（Function）、函数块（Function Block）
//////PROGRAM：用于定义主程序、控制逻辑等。通常包含主要的执行逻辑，可能包括调用函数块、函数等
//////FUNCTION：用于定义函数，包括输入参数、输出参数 (Variable) 和计算逻辑 (Expression & Statements)。函数可以被调用，但在函数内不能包含其他程序单元的定义
//////FUNCTION_BLOCK：用于定义函数块，其中包括输入、输出、局部变量和一个或多个方法 (Method)。函数块通常设计为可重用的功能单元，需要被实例化之后才能调用
//////标准情况下，FUNCTION 和 FUNCTION_BLOCK 可以在 PROGRAM 中被调用，但不会在 FUNCTION 或 FUNCTION_BLOCK 内部包含其他 FUNCTION 或 FUNCTION_BLOCK 的定义。
 */

grammar ST;

program                             : all_decl+;
all_decl                            : using_directive* ( namespace_decl | var_global_decls | data_type_decl | prog_decl | func_decl | fb_decl | class_decl | interface_decl)+;

/* 命名空间Namespace */
namespace_decl                      : 'NAMESPACE' 'INTERNAL' ? namespace_h_name using_directive * namespace_elements 'END_NAMESPACE'; 

namespace_elements                  : ( data_type_decl | func_decl | fb_decl | class_decl | interface_decl | namespace_decl )+; 
namespace_h_name                    : namespace_name ( '.' namespace_name )*; 
namespace_name                      : Identifier; 

using_directive                     : 'USING' namespace_h_name ( ',' namespace_h_name )* ';'; 

/* 程序体Program */
prog_decl                           : 'PROGRAM' prog_name all_var_decls* method_decl* 'BEGIN'? statements 'END_PROGRAM';
prog_name                           : Identifier;

/* 函数Function */
func_decl                           : 'FUNCTION' derived_func_name (':' data_type_access)? using_directive* all_var_decls* method_decl* 'BEGIN'? statements 'END_FUNCTION';

func_name                           : std_func_name | derived_func_name; 
std_func_name                       : 'TRUNC' | 'ABS' | 'SQRT' | 'LN' | 'LOG' | 'EXP' | 'SIN' | 'COS' | 'TAN' | 'ASIN' | 'ACOS' | 'ATAN' | 'ATAN2 ' | 'ADD' | 'SUB' | 'MUL' | 'DIV' | 'MOD' | 'EXPT' | 'MOVE ' | 'SHL' | 'SHR' | 'ROL' | 'ROR' 
                                    | 'AND' | 'OR' | 'XOR' | 'NOT' | 'SEL' | 'MAX' | 'MIN' | 'LIMIT' | 'MUX ' | 'GT' | 'GE' | 'EQ' | 'LE' | 'LT' | 'NE' | 'LEN' | 'LEFT' | 'RIGHT' | 'MID' | 'CONCAT' | 'INSERT' | 'DELETE' | 'REPLACE' | 'FIND'; 
derived_func_name                   : Identifier;

func_call                           : func_access '(' ( param_assign ( ',' param_assign )* )? ')'; 
func_access                         : ( namespace_name '.' )* func_name;

/* 函数块Function_block */
fb_decl                             : 'FUNCTION_BLOCK' ( 'FINAL' | 'ABSTRACT' )? derived_fb_name using_directive * ( 'EXTENDS' ( type_access ) )? ( 'IMPLEMENTS' interface_name_list )?
                                      all_var_decls*  method_decl* statements  'END_FUNCTION_BLOCK'; 
fb_name                             : std_fb_name | derived_fb_name; 
std_fb_name                         : 'SR' | 'RS' | 'R_TRIG' | 'F_TRIG' | 'CTU'| 'CTD' | 'CTUD' | 'TP' | 'TON' | 'TOF'; 
derived_fb_name                     : Identifier; 

fb_instance_name                    : ( namespace_name '.' )* fb_name '^' *; 

/* 方法Method */
method_decl                         : 'METHOD' Access_Spec ( 'FINAL' | 'ABSTRACT' )? 'OVERRIDE' ? Identifier ( ':' data_type_access )?
                                    ( all_var_decls )* 'BEGIN'?  statements 'END_METHOD'; 

/* 类Class */
class_decl                          : 'CLASS' ( 'FINAL' | 'ABSTRACT' )? Identifier using_directive * ( 'EXTENDS' type_access )? ( 'IMPLEMENTS' interface_name_list )?
                                    ( all_var_decls )* ( method_decl )* 'END_CLASS'; 
class_instance_name                 : ( namespace_name '.' )* Identifier '^' *; 
interface_decl                      : 'INTERFACE' Identifier using_directive *
                                    ( 'EXTENDS' interface_name_list )? method_prototype * 'END_INTERFACE'; 
method_prototype                    : 'METHOD' Identifier ( ':' data_type_access )? all_var_decls * 'END_METHOD'; 
interface_spec_init                 : variable_list ( ':=' interface_value )?; 
interface_value                     : symbolic_variable | fb_instance_name | class_instance_name | 'NULL'; 
interface_name_list                 : type_access ( ',' type_access )*; 
access_spec                         : 'PUBLIC' | 'PROTECTED' | 'PRIVATE' | 'INTERNAL';


/*表达式及语句expression & statement */

//expression部分
//由于EMF元模型中的expression结构最多包含两个expression，使用以下规则模式，对AST进行规约，可以将长表达式分解多层短表达式
expression                          : '(' expression ')'
                                    | ( '+' | '-' ) expression
                                    | expression '*' expression
                                    | expression ( '+' | '-' ) expression
                                    | expression ('<' | '>' | '<=' | '>=') expression
                                    | expression ('=' | '<>') expression
                                    | expression '**' expression
                                    | constant
                                    | var_access
                                    | func_call
                                    | ref_value;



//statement部分
statements                          : stmt_list;
stmt_list                           : ( stmt ? ';' )*; 
stmt                                : assign_stmt | subprog_ctrl_stmt | selection_stmt | iteration_stmt | exit_stmt | continue_stmt; 

fragment Multibit_part_access       : '.' ( Unsigned_Int | '%' ( 'X' | 'B' | 'W' | 'D' | 'L' ) ? Unsigned_Int ); 

assign_stmt                         : ( variable ':=' expression ) | ref_assign | assignment_attempt; 
assignment_attempt                  : ( ref_name | ref_deref ) '?=' ( ref_name | ref_deref | ref_value ); 

invocation                          : ( fb_instance_name | type_name | 'THIS' | ( ( 'THIS' '.' )? ( ( ( fb_instance_name | class_instance_name ) '.' )+ ) type_name ) ) '(' ( param_assign ( ',' param_assign )* )? ')'; 
subprog_ctrl_stmt                   : func_call | invocation | 'SUPER' '(' ')' | 'RETURN'; 
param_assign                        : ( ( variable_name ':=' )? expression ) | ref_assign | ( 'NOT' ? variable_name '=>' variable ); 

selection_stmt                      : if_stmt | case_stmt; 

if_stmt                             : 'IF' expression 'THEN' stmt_list elsif_stmt* else_stmt? 'END_IF'; 
elsif_stmt                          : 'ELSIF' expression 'THEN' stmt_list;
else_stmt                           : 'ELSE' stmt_list;

case_stmt                           : 'CASE' expression 'OF' case_selection + ( 'ELSE' stmt_list )? 'END_CASE'; 
case_selection                      : case_list ':' stmt_list; 
case_list                           : case_list_elem ( ',' case_list_elem )*; 
case_list_elem                      : subrange | expression; 

iteration_stmt                      : for_stmt | while_stmt | repeat_stmt;

for_stmt                            : 'FOR' control_variable ':=' for_list 'DO' stmt_list 'END_FOR'; 
control_variable                    : Identifier; 
for_list                            : start_expr 'TO' end_expr ( 'BY' step_expr )?; 
start_expr                          : expression;
end_expr                            : expression;
step_expr                           : expression;

while_stmt                          : 'WHILE' expression 'DO' stmt_list 'END_WHILE'; 

repeat_stmt                         : 'REPEAT' stmt_list 'UNTIL' expression 'END_REPEAT';

exit_stmt                           : 'EXIT';
continue_stmt                       : 'CONTINUE';

/* 数据类型 */
data_type_access                    : elem_type_name | derived_type_access;

//基本数据类型
elem_type_name                      : Numeric_Type_Name | Char_Type_Name | string_Type_Name | Bit_Str_Type_Name | Date_Type_Name | Time_Type_Name; 
Numeric_Type_Name                   : Int_Type_Name | Real_Type_Name; 
Int_Type_Name                       : Sign_Int_Type_Name | Unsign_Int_Type_Name; 
Sign_Int_Type_Name                  : 'SINT' | 'INT' | 'DINT' | 'LINT'; 
Unsign_Int_Type_Name                : 'USINT' | 'UINT' | 'UDINT' | 'ULINT';
Real_Type_Name                      : 'REAL' | 'LREAL'; 
Char_Type_Name                      : 'CHAR' | 'WCHAR'; 
string_Type_Name                    : 'STRING' ( '[' Unsigned_Int ']' )? | 'WSTRING' ( '[' Unsigned_Int ']' )? ;
Time_Type_Name                      : 'TIME' | 'LTIME'; 
Date_Type_Name                      : 'DATE' | 'LDATE'; 
Tod_Type_Name                       : 'TIME_OF_DAY' | 'TOD' | 'LTOD'; 
DT_Type_Name                        : 'DATE_AND_TIME' | 'DT' | 'LDT'; 
Bit_Str_Type_Name                   : Bool_Type_Name | Multibits_Type_Name; 
Bool_Type_Name                      : 'BOOL'; 
Multibits_Type_Name                 : 'BYTE' | 'WORD' | 'DWORD' | 'LWORD';

//用户自定义数据类型及初始化
derived_type_access                 : type_access | string_type_access; 
string_type_access                  : ( namespace_name '.' )* string_Type_Name; 
type_access                         : ( namespace_name '.' )* type_name;
type_name                           : Identifier;

data_type_decl                      : 'TYPE' type_decl_list 'END_TYPE'; 
type_decl_list                      : ( type_decl ';' )+;
type_decl                           : simple_type_decl | subrange_type_decl | enum_type_decl | array_type_decl | struct_type_decl | str_type_decl | ref_type_decl; 

/* ///这里注意一个问题，影响到emf的ECore编写
//////类型声明是独立的，初始化属于变量声明的部分
//////对原规则做出修改，在类型声明中去除init
///////////////////////////////////////////// */
simple_type_decl                    : type_name ':' elem_type_name;

simple_spec_init                    : elem_type_name (':=' expression )?; 

//子范围数据类型定义
subrange_type_decl                  : type_name ':' subrange_spec;
subrange_spec                       : type_access | elem_type_name '('subrange')' ; 
subrange                            : expression '..' expression; 

subrange_spec_init                  : subrange_spec ( ':=' expression )?; 

// 枚举定义
enum_type_decl                      : type_name ':' enum_spec;

enum_spec                           : type_access 
                                    | '(' enum_value ( ',' enum_value )* ')'
                                    | elem_type_name? '(' named_spec ( ',' named_spec )* ')';

named_spec                          : enum_value ( ':=' expression )?;
quote_value                         : ( type_name '#' )? enum_value; 
enum_value                          : Identifier;

enum_spec_init                      : enum_spec (':=' quote_value)?;

// 数组定义
array_type_decl                     : type_name ':' array_spec;
array_spec                          : type_access
                                    | 'ARRAY' '[' subrange ( ',' subrange)* ']' 'OF' data_type_access;

array_spec_init                     : array_spec (':=' array_init)?;


array_init                          : '[' array_elem_init_value (',' array_elem_init_value)* ']'
                                    | '(' array_elem_init_value ( ',' array_elem_init_value)* ')'
                                    | '[' Unsigned_Int array_elem_init_value? ']'
                                    | '(' Unsigned_Int array_elem_init_value? ')';

array_elem_init_value               : expression | struct_init | array_init;

//结构体定义
struct_type_decl                    : type_name ':' struct_spec;
struct_spec                         : type_access
                                    | 'STRUCT' 'OVERLAP'? (struct_elem_decl';')+ 'END_STRUCT';
struct_elem_decl                    :struct_elem_name ( 'AT' Direct_Variable Multibit_part_access ? )? ':' (data_type_access | subrange_spec | enum_spec | array_spec | struct_spec);


struct_spec_init                    : struct_spec (':=' struct_init)?;
struct_init                         : '(' struct_elem_init ( ',' struct_elem_init )* ')';
struct_elem_init                    : struct_elem_name ':=' ( expression | array_init | struct_init | ref_value ); 

struct_elem_name                    : Identifier; 

//字符串定义
str_type_decl                       : string_Type_Name ':' string_Type_Name ( ':=' Char_Str )?;

/* 引用操作符 */
ref_type_decl                       : Identifier ':' ref_spec_init; 
ref_spec_init                       : ref_spec ( ':=' ref_value )?; 
ref_spec                            : 'REF_TO' + data_type_access; 
ref_name                            : Identifier; 
ref_value                           : ref_addr | 'NULL'; 
ref_addr                            : 'REF' '(' ( symbolic_variable | fb_instance_name | class_instance_name ) ')'; 
ref_assign                          : ref_name ':=' ( ref_name | ref_deref | ref_value ); 
ref_deref                           : ref_name '^' +;



/* 变量声明及初始化 */
all_var_decls                       : var_decls | io_var_decls | var_external_decls | var_global_decls | var_temp_decls | var_access_decls | var_local_decls | var_local_partly_decl;
io_var_decls                        : var_input_decls | var_output_decls | var_in_out_decls;

//变量声明，这里使用了同一种匹配规则，而SECTIONS和QUALIFIERS的搭配情况较为复杂，这里只考虑文本的解析，不考虑搭配规则
var_decls                           :'VAR'           (Is_Retain | Access_Spec)?  ( decl_common_part ';' )* 'END_VAR';
var_input_decls                     :'VAR_INPUT'     (Is_Retain | Access_Spec)?  ( decl_common_part ';' )* 'END_VAR';
var_output_decls                    :'VAR_OUTPUT'    (Is_Retain | Access_Spec)?  ( decl_common_part ';' )* 'END_VAR';
var_in_out_decls                    :'VAR_IN_OUT'    (Is_Retain | Access_Spec)?  ( decl_common_part ';' )* 'END_VAR';
var_external_decls                  :'VAR_EXTERNAL'  (Is_Retain | Access_Spec)?  ( decl_common_part ';' )* 'END_VAR';
var_global_decls                    :'VAR_GLOABLE'   (Is_Retain | Access_Spec)?  ( decl_common_part ';' )* 'END_VAR'
                                    |'VAR_GLOABLE'   (Is_Retain | Access_Spec)?  ( variable_name 'AT' Direct_Variable) 'END_VAR';
var_temp_decls                      :'VAR_TEMP'      (Is_Retain | Access_Spec)?  ( decl_common_part ';' )* 'END_VAR';
var_access_decls                    :'VAR_ACCESS'    (Is_Retain | Access_Spec)?  ( decl_common_part ';' )* 'END_VAR';

var_local_decls                     :'VAR' ( 'CONSTANT' | 'RETAIN' | 'NON_RETAIN' )? ( loc_var_decl ';' )* 'END_VAR'; 
loc_var_decl                        : variable_name ? 'AT' Direct_Variable ':' loc_var_spec_init; 
loc_var_spec_init                   : simple_spec_init | array_spec_init | struct_spec_init; 

var_local_partly_decl               : 'VAR' ( 'RETAIN' | 'NON_RETAIN' )? loc_partly_var * 'END_VAR';
loc_partly_var                      : variable_name 'AT' '%' ( 'I' | 'Q' | 'M' ) '*' ':' var_spec ';'; 
var_spec                            : elem_type_name | array_spec | type_access | ( 'STRING' | 'WSTRING' ) ( '[' Unsigned_Int ']' )?;

/* decl_common_part                    :variable_list ':' (simple_spec_init | str_var_init | ref_spec_init | array_spec_init | struct_spec_init | edge_decl | unknown_decl)
                                    | interface_spec_init; */
decl_common_part                    :variable_list ':' (simple_spec_init | subrange_spec_init | enum_spec_init | ref_spec_init | array_spec_init | struct_spec_init | unknown_decl)
                                    | interface_spec_init;

//变量初始化
/* edge_decl                           :  'BOOL' ( 'R_EDGE' | 'F_EDGE' );  */

//unknown_decl用于调试使用，测试用例中可能包含未声明的用户定义类型
unknown_decl                       : type_access ( ':=' ( expression | array_init | struct_init ))?;



variable                            : Direct_Variable | symbolic_variable;
variable_list                       : variable_name (',' variable_name)*;
variable_name                       : Identifier;
multi_elem_var                      : var_access ( subscript_list | struct_variable )+; 
subscript_list                      : '[' subscript ( ',' subscript )* ']'; 
subscript                           : expression; 
struct_variable                     : '.' struct_elem_select; 
struct_elem_select                  : var_access; 


var_access                          : variable_name | ref_deref;
symbolic_variable                   : ( ( 'THIS' '.' ) | ( namespace_name '.' )+ )? ( var_access | multi_elem_var ); 
// 直接取址变量
// 注意可能会和ID冲突,所以需要 fragment Direct_variable 
fragment Direct_Variable            : '%' ( 'I' | 'Q' | 'M' ) ( 'X' | 'B' | 'W' | 'D' | 'L' )? Unsigned_Int ( '.' Unsigned_Int )*;

//以下七种Qualifiers，为了避免冲突，一般只使用其中一个，不会多个关键字同时使用
Is_Retain                           : 'CONSTANT' | 'RETAIN' | 'NONRETAIN';
Access_Spec                         : 'PUBLIC' | 'PROTECTED' | 'PRIVATE' | 'INTERNAL';




/* 常量 */
constant                            : numeric_literal | char_literal | time_literal | bit_str_literal | bool_literal;

//数字类常量
//这里为了减少语法树的深度，将部分RuleNode改写成了TerminalNode
numeric_literal                     : int_literal | Real_Literal;

int_literal                         : ( Int_Type_Name '#' )? (Unsigned_Int | Signed_Int | Binary_Int | Octal_Int | Hex_Int);
Real_Literal                        : ( Real_Type_Name '#' )? Signed_Int '.' Unsigned_Int (( 'E' | 'e' ) Signed_Int)?;
bool_literal                        : 'FALSE' | 'TRUE';
bit_str_literal                     : ( Multibits_Type_Name '#' )? ( Unsigned_Int | Binary_Int | Octal_Int | Hex_Int );

Unsigned_Int                        : Digit ( '_' ? Digit)*;
Signed_Int                          : ( '+' | '-' )? Unsigned_Int;
Binary_Int                          : '2#' ( '_' ? Bit )+;
Octal_Int                           : '8#' ( '_' ? Octal_Digit )+;
Hex_Int                             : '16#' ( '_' ? Hex_Digit )+;


//字符类常量
char_literal                        : ( 'STRING#' )? Char_Str;
Char_Str                            : S_Byte_Char_Str | D_Byte_Char_Str;
S_Byte_Char_Str                     : '\'' S_Byte_Char_Value + '\'';
D_Byte_Char_Str                     : '"' D_Byte_Char_Value + '"';
fragment S_Byte_Char_Value          : Common_Char_Value | '$\'' | '"' | '$' Hex_Digit Hex_Digit;
fragment D_Byte_Char_Value          : Common_Char_Value | '\'' | '$"' | '$' Hex_Digit Hex_Digit Hex_Digit Hex_Digit;
fragment Common_Char_Value          :  '!' | '#' | '%' | '&' | '('..'/' | '0'..'9' | ':'..'@' | 'A'..'Z' | '['..'`' | 'a'..'z' | '{'..'~' | '$$' | '$L' | '$N' | '$P' | '$R' | '$T';

//时间常量
time_literal                        : Duration | Time_Of_Day | Date | Date_And_Time;
Duration                            : ( Time_Type_Name | 'T' | 'LT' ) '#' ( '+' | '-' )? Interval;
Fix_Point                           : Unsigned_Int ( '.' Unsigned_Int )?;
fragment Interval                   : Days | Hours | Minutes | Seconds | Milliseconds | Microseconds | Nanoseconds;
fragment Days                       : ( Fix_Point 'd' ) | ( Unsigned_Int 'd' '_' ? )? Hours ?; 
fragment Hours                      : ( Fix_Point 'h' ) | ( Unsigned_Int 'h' '_' ? )? Minutes ?; 
fragment Minutes                    : ( Fix_Point 'm' ) | ( Unsigned_Int 'm' '_' ? )? Seconds ?; 
fragment Seconds                    : ( Fix_Point 's' ) | ( Unsigned_Int 's' '_' ? )? Milliseconds ?; 
fragment Milliseconds               : ( Fix_Point 'ms' ) | ( Unsigned_Int 'ms' '_' ? )? Microseconds ?; 
fragment Microseconds               : ( Fix_Point 'us' ) | ( Unsigned_Int 'us' '_' ? )? Nanoseconds ?; 
Nanoseconds                         : Fix_Point 'ns'; 
Time_Of_Day                         : ( Tod_Type_Name | 'LTIME_OF_DAY' ) '#' Daytime; 
Daytime                             : Day_Hour ':' Day_Minute ':' Day_Second; 
Day_Hour                            : Unsigned_Int; 
Day_Minute                          : Unsigned_Int; 
Day_Second                          : Fix_Point; 
Date                                : ( Date_Type_Name | 'D' | 'LD' ) '#' Date_Literal; 
Date_Literal                        : Year '-' Month '-' Day; 
Year                                : Unsigned_Int; 
Month                               : Unsigned_Int; 
Day                                 : Unsigned_Int; 
Date_And_Time                       : ( DT_Type_Name | 'LDATE_AND_TIME' ) '#' Date_Literal '-' Daytime;


/* 标识符 */
Identifier                          : Letter  (Letter | Digit)*;
fragment Letter                     : [A-Za-z] | '_';
fragment Digit                      : '0'..'9';
fragment Bit                        : '0'..'1';
fragment Octal_Digit                : '0'..'7';
fragment Hex_Digit                  : '0'..'9' | 'A'..'F';
/* Pragma 跳过*/
Pragma                              : '{' .*? '}' ->skip;

/* 关于跳过注释以及换行制表符等 */
LINE_COMMENT                        : '//' ~[\r\n]* -> skip;
MULTI_COMMENT_1                     : '/' '*' .*? '*' '/' -> skip;
MULTI_COMMENT_2                     : '(' '*' .*? '*' ')' -> skip;
WS                                  : ( ' ' | '\t' | '\r' | '\n' ) ->skip;
EOL                                 : '\n';