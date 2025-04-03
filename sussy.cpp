/*
sus lang compiler
compile windows (user lib probably not needed):
time clang sussy.cpp -std=c++17 -g -luser32.lib -Wno-deprecated-declarations -o sussy.exe
or build.sh
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdint.h>

/// TYPE ALIASES START
using u8 = unsigned char;
using i8 = char;
using u32 = unsigned int;
using u64 = unsigned long long int;
using i32 = int;
using i64 = unsigned long long int;
using f32 = float;
using f64 = double;

/// COMMON STRUCTS AND FUNCTIONS START

#define ASSERT(cond) do { if (!(cond)) { printf("Assert at %s:%d (%s) - %s\n",__FILE__, __LINE__, __func__, #cond); fflush(stdout); /* *(int*)(0) = 0;*/ __builtin_trap(); } } while (0)
//#define ASSERT()

// defer lambda hack
#define CONCAT(x, y) x##y
#define CONCAT_LINE_PREPROC(x, y) CONCAT(x, y)
#define CONCAT_LINE(x) CONCAT_LINE_PREPROC(x, __LINE__)
template<typename F> struct _defer { F f; _defer(F f) : f(f) {}; ~_defer() { f();} };
#define defer _defer CONCAT_LINE(_defer_) = [&]()

struct buf {
    u8* data;
    u64 size;
};
// sized string macro, use as printf("as string %.*s", SS(my_buf))
#define SS(b) (int)(b).size, (b).data

struct arena {
    u8* data; // start
    u8* curr;
    u64 size;
    // todo: make locked_by work in opposite way so if we pushed/popped it should be ok
    u8* locked_by; // if inside push_start<->push_end, just to checks valid use
    u8* max_usage; // for stats, todo: make optional?
    // u64 counter; // some stuff for stats and keeping track for valid use??
} glob; // todo: pull of glob

u8* push(arena* a, u64 size) { // todo: alignment
    ASSERT(a->locked_by == 0); // use push_next if arena in push mode
    u8* out = a->curr;
    a->curr += size;
    ASSERT(a->curr <= a->data + a->size); // todo: grow alloc
    if (a->curr > a->max_usage) a->max_usage = a->curr;
    return out;
}
u8* pop_to(arena* a, void* addr) {
    a->curr = (u8*)addr;
    return (u8*)addr;
}

// todo: not a fun how it looks yet
u8* push_start(arena* a) {
    ASSERT(a->locked_by == 0);
    a->locked_by = a->curr;
    return a->curr;
}
u8* push_next(arena* a, void* locked_by, i64 size) {
    ASSERT(locked_by == a->locked_by);
    a->curr += size;
    ASSERT(a->curr <= a->data + a->size); // not growing here for sure
    return a->curr;
}
void push_end(arena* a, void* locked_by) {
    ASSERT(locked_by == a->locked_by);
    if (a->curr > a->max_usage) a->max_usage = a->curr;
    a->locked_by = 0;
}

#define TO_MB(bytes) ((bytes) / ((f64)1024 * 1024))

arena arena_new(u64 size) {
    arena a = {};
    a.size = size;
    a.data = a.curr = a.max_usage = (u8*)calloc(1, a.size);
    a.locked_by = 0;
    return a;
}

u8* arena_reset(arena *a) { // resets all allocation and sets curr = data
    a->curr = a->data;
    return a->curr; //maybe not needed
}

void arena_delete(arena *a) {
    if (a->data != nullptr)
        free(a->data);
    *a = {};
}

// available size
#define ARENA_AVAIL(a) ((a).size - ((a).curr - (a).data))

buf str(const char* cstr) { // buf view
    return {(u8*)cstr, strlen(cstr)};
}

// unused?
buf strc(const char* cstr) { // creates a buf with a copy to glob
    u64 size = strlen(cstr);
    buf out = { push(&glob, size + 1), size };
    memcpy(glob.curr, cstr, size + 1); // keeping 0 term but not reflecting it in 'size'
    return {(u8*)glob.curr, size};
}

buf str(arena* a, const char* fmt, ...) {
    buf out = {};

    u64 avail = ARENA_AVAIL(*a);
    va_list args;
    va_start(args, fmt);
    int pos = vsnprintf((char*)a->curr, avail - 1, fmt, args);
    va_end(args);
    
    if (pos > 0) {
        out = {push(a, pos + 1), (u64)pos };
        out.data[out.size] = 0;
    }
    return out;
}

bool eq(const char* str, const char* str2) {
    u32 size = strlen(str);
    if (size != strlen(str2)) return false;
    return memcmp(str, str2, size) == 0;
}
bool eq(const char* str, buf str2) {
    if (strlen(str) != str2.size) return false;
    return memcmp(str, str2.data, str2.size) == 0;
}
bool eq(buf str, buf str2) {
    if (str.size != str2.size) return false;
    return memcmp(str.data, str2.data, str.size) == 0;
}

buf read_file(buf input_file_name, int zero_padding_bytes, arena* a) { // todo: allocator
    buf f = {};
	FILE* file = fopen((const char*)input_file_name.data, "rb");
	if (file != NULL) {
		fseek(file, 0, SEEK_END);
		f.size = ftell(file);
		fseek(file, 0, SEEK_SET);

		f.data = push(a, f.size + zero_padding_bytes); // todo: assert temp size

		u64 read_count = fread(f.data, f.size, 1, file);
        for (int i = 0; i < zero_padding_bytes; i++)
            f.data[f.size + i] = 0;
		fclose(file);
	}

	return f;
}
/*
void write_file(const char* filename, buffer buf) {
    FILE* file = fopen(filename, "wb");
    if (file != NULL) {
        u64 write_count = fwrite(buf.data, 1, buf.size, file);
        ASSERT(write_count != 0);
        fclose(file);
    }
}
*/

struct ivec2 {
    i32 x, y;
};

/// COMPILER START

// COMPILER LEX TOKEN START
// todo: idea: don't push LEX_SEPARATOR to the token list, only when it's needed (in math expression precedence) - but then
//    how do lexer know it's inside a math expression? (keep track of scope and see if it's between '=' and new line??
//    but what about right hand side then?
// values just for debugging, also some stuff could be condensed into larger groups like punctuation, separators, operations
enum lex_token_t {
    LEX_UNKNOWN_ZERO = 0, // ?? idk zero initilization good ??
    LEX_UNKNOWN = '?',    // empty
    LEX_COMMA = ',', // could be a separator?
    LEX_DOT = '.',
    LEX_NAME = 'n', // identifier name, function/variable, keyword??
    LEX_COMMENT = 'c', // comment '//' or '/* */'
    LEX_SEPARATOR = 's', // spaces, tabs, '\r', new lines
    LEX_END_EXP = ';', // ';' - not mandatory
    LEX_DEFINITION = ':', // :: definition
    LEX_OPERATION = 'O', // =, *, -, +, \, %, !, ~, &&, ||, &, |, ==, <, >, <=, >=, shifts << >> (not handling <<=, >>=)
    LEX_SCOPE = '^', // {}, (), []
    LEX_NUMBER = '1', // number constants
    // todo: char, string, raw string, '? ternary', -> ' <-
};

struct lex_token {
    buf src;
    lex_token_t type; // todo: move to be a first member to init with lex_token_t ...
    ivec2 location; // inline number(.x) and line number (.y)
    i32 new_lines; // what for? 
    // todo: more metadata?
};

#define NUM_1ST(c) ((c) >= '0' && (c) <= '9')
#define NUM_2ND(c) (NUM_1ST(c) || (c) == '\'' || (c) == '_')
// todo: emoji support lol, after inventing encoding lol
// todo: maybe allow numbers as first char?
// is start of identifier, cannot include numbers or other symbols except '_'
#define IDT_1ST(c) (((c) >= 'A' && (c) <= 'Z') || ((c) >= 'a' && (c) <= 'z') || (c) == '_')
// is characted belongs to identifier, can include numbers as non first character
#define IDT_2ND(c) (IDT_1ST((c)) || NUM_1ST(c))

/// COMPILER PARSING START

struct type_desc {
    enum kind_t {
        ERROR_TEMP, // 0 ?
        BUILTIN, // u32, i32, f32 ... todo: BUILTIN_U32, BUILDIN_F32..., BUILTIN TYPE??
        // TYPE??
        // todo: implement the rest
        PTR, // pointer
        STRUCT, // struct/defined function
        FUNCTION, // is function a pointer?
    } kind;
    u32 size_of;
    buf name;
    // todo: desc union
    // BUILTIN
    //     BUILDIN INT
    bool is_signed;
    //     BUILTIN FLOAT
    bool is_float;
    // PTR
    type_desc* type_of_pointed;
    // STRUCT
    type_desc* member;
    // todo: member names
    // todo: default values
    // FUNCTION
    type_desc* function_params;
    u32 function_params_size;
    type_desc* return_params; // 1 for now
};
type_desc builtin_u32 = {
    type_desc::BUILTIN,
    sizeof(u32),
    str("u32"),
    false,
    false
};

enum scope_t { // just for checking what kind of expressions are allowed?
    SCOPE_NONE, // todo: leave file = 0?
    SCOPE_FILE,
    SCOPE_FUNC,
    SCOPE_EXPR, // if, for, while, right hand side of = ...
};
enum ast_t {
    //AST_PUSH_SCOPE, ??
    //AST_TOPLEVEL?
    //AST_COMPOUND/AST_SEQUENCE?
    AST_NONE, // todp: remove?
    AST_VAR_INIT = 'i', // i::type; i::type=1; i :: struct_type; also covers function parameters
    // todo: remove AST_VAR, that should just be a reference to another data structure
    AST_VAR = 'v', // leaf reference to variable or constant
    AST_OPERATION = 'o', // asignment, multiplication, add
    AST_EXPRESSION = 'e', // unused for now
};

enum operation_t {
    OP_ASSIGN,
    OP_MUL,
    OP_DIV,
    OP_ADD,
    OP_SUB,
};

struct AST {
    ast_t type;
    lex_token* start_t;
    lex_token* end_t;
    scope_t scope_type;
    // todo: desc union
    // AST_VAR AST_VAR_INIT
    // todo: copy name here or start_t is enough?
    type_desc* var_type;
    bool is_constant; // number literal for now...
    bool is_function_param; // only AST_VAR_INIT
    // AST_OPERATION
    AST* left;
    AST* right; // +AST_VAR_INIT
    operation_t op_type;
    // AST_EXPRESSION
    AST* expressions;
    u32 expressions_count;
};

void comp(buf input_file_name) {
    // todo: lexing + parsing could be done at the same time since they can work as a stream?
    bool debug_print_file = false;
    bool do_lexing = true; bool debug_print_lexing = false;
    bool do_parsing = false; bool debug_print_parsing = true;
    bool do_bytecode = false; bool debug_print_bytecode = true;

    // todo: assert utf8/ascii encoding or something
    buf input = read_file(input_file_name, 10, &glob); // we can read a few bytes past the end of the file and be fine
    if (debug_print_file) {
        printf("input:\n %.*s\n", SS(input));
        printf("input size: %d\n", (int)input.size);
    }

    arena scratch = arena_new(64 * 1024); // a small thing, just for errors?
    defer { arena_delete(&scratch); }; // maybe we don't care?

    lex_token* all_tokens = 0;
    u32 all_tokens_size = 0;
    buf lex_error = {}; // todo: add a proper error type enum ?
    
    //// step 1. lexing - convert text to a lex token list
    if (do_lexing) {
        all_tokens = (lex_token*)push_start(&glob);
        defer { push_end(&glob, all_tokens); };

        u8 *c = input.data;
        i32 lines_total = 1; // need to keep a line number for error msg and other stuff
        u8* line_start = c;

        while (c < input.data + input.size) {
            lex_token t = {{c, 0}, LEX_UNKNOWN};
            t.location.x = c - line_start + 1; // +1 to start from 1 
            t.location.y = lines_total;
            i32 token_new_lines = 0;
            #define NEWLINE() do { c++; token_new_lines++; line_start = c; } while(0)
            
            switch (*c) {
                case ',': t.type = LEX_COMMA; c++; break;
                case ';': t.type = LEX_END_EXP; c++; break;
                case '.': t.type = LEX_DOT; c++; break;
                // todo: combine multiple sequential separators into a single token with length
                case '\n': t.type = LEX_SEPARATOR; NEWLINE(); break;
                // todo: ignore \r completely?
                case ' ': case '\r': case '\t': t.type = LEX_SEPARATOR; c++; break;
                case '{': case '}': case '(': case ')': case '[': case ']': t.type = LEX_SCOPE; c++; break;
                case '*': case '+': case '-': case '%': case '!': case '~': {
                    t.type = LEX_OPERATION;
                    c++; 
                    if (*c == '=') c++;  // +=, -=, != except >=, <= which handled below
                } break;
                // =, ==, &, &&, |, ||, >, <, >=, <=, <<, >> handle with pairs, no <<=, >>= because I never use those
                case '=': case '&': case '|': case '>': case '<':
                t.type = LEX_OPERATION;
                if (c[1] == *c || c[1] == '=') c += 2; // pairs ==, &&, << ... and <=, >=, &=, |=
                else c++;
                break;
                case '/': { // comment or division operation
                    if (c[1] == '/') { // 1. can be a // comment
                        t.type = LEX_COMMENT;
                        while(*c != '\n' && *c != 0) c++;
                        // no NEWLINE(); here since we want it explicitly to check end of expression
                        break;
                    }
                    if (c[1] == '*') { // 2. can be a /* */ comment
                        t.type = LEX_COMMENT;
                        // todo: nested comments !!
                        c += 2; // revind to the char past '*'
                        while (!(c[0] == '*' && c[1] == '/') && *c != 0) {
                            if (*c == '\n') NEWLINE();
                            else c++;
                        }
                        c += 2; // skip past the last '/'
                        break;
                    }
                    t.type = LEX_OPERATION; // 3. can be a division in an expression
                    c++;
                    if (*c == '=') c++; // /=
                } break;
                case ':': {
                    if (c[1] == ':') {
                        t.type = LEX_DEFINITION;
                        c++;
                    } else {
                        lex_error = str("single ':', but '::' expected");
                    }
                    c++;
                } break;
                default: {
                    if (IDT_1ST(*c)) { // consume identifier name, '6i' works for now where 6 is ignored
                        t.type = LEX_NAME;
                        while(IDT_2ND(*c)) c++;
                        break;
                    }
                    if (NUM_1ST(*c)) { // todo: 0x/0b...
                        t.type = LEX_NUMBER;
                        while (NUM_2ND(*c)) c++;
                        break;
                    }
                    lex_error = str(&scratch, "unknown char [%c]", *c);
                    c++; 
                } break;
            }
            
            lines_total += token_new_lines;
            
            t.src.size = c - t.src.data;
            t.new_lines = token_new_lines;
            
            all_tokens[all_tokens_size] = t;
            push_next(&glob, all_tokens, sizeof(lex_token));
            all_tokens_size++;
            
            if (debug_print_lexing) {
                if (t.src.data[0] == '\r') printf("[%c]\\r", t.type);
                else printf("[%c]%.*s", t.type, SS(t.src));
            }
            if (lex_error.size != 0) {
                printf("syntax error at %d:%d: %.*s\n", t.location.y, t.location.x, SS(lex_error));
                // todo: set some error state to return/signal?
                break; // todo: some stuff can be ignored or that's a bad idea?
            }
        }

        // make a dummy zero-filled token for easy OOB check in the next step (todo: add multiple?)
        push_next(&glob, all_tokens, sizeof(lex_token));
        all_tokens[all_tokens_size] = {};

        printf("\nstep 1. lexing %s\n", lex_error.size == 0 ? "successful" : "failed");
    }
    if (lex_error.size == 0) {
        do_parsing = true;
    }

    printf("token count %d\n", all_tokens_size);

    // todo: out of order wait list?

    // def list - functions and structs/enums
    // todo: do a proper grow without MAX_DEF
    AST *function_start = 0; // main func for now (always at start of all_ast??)
    
    // baking memory for ast list
    AST* all_ast = 0;
    u32 all_ast_count = 0;

    buf parse_error = {}; // todo: add a proper error type enum ?

    //// step 2. convert lex tokens into AST
    if (do_parsing) {
        // todo: just use arena and grow, need arena pool tho first
        #define MAX_SCOPE 128
        scope_t scope_stack[MAX_SCOPE] = { SCOPE_FILE, SCOPE_NONE }; // init first scope with FILE
        int scope_i = 0;
        
        #define PUSH_SCOPE(TYPE) do { printf("(begin scope (%d) #%d)", TYPE, scope_i + 1); ASSERT(scope_i >= 0 && scope_i < MAX_SCOPE - 1); scope_i++; scope_stack[scope_i] = TYPE; } while(0)
        #define POP_SCOPE() do { printf("(end scope (%d) #%d)", scope_stack[scope_i], scope_i); ASSERT(scope_i > 0); scope_stack[scope_i] = SCOPE_NONE; scope_i--; } while (0)

        all_ast = (AST*)push_start(&glob);
        defer { push_end(&glob, all_ast); };
        
        #define PUSH_AST(A) do { all_ast[all_ast_count] = (A); push_next(&glob, all_ast, sizeof(AST)); all_ast_count++;\
                                 if (debug_print_parsing) printf("A[%c]", (A).type); } while (0)

        lex_token *t = all_tokens;
    
        // todo: make into a proper functions not a macro?
        #define SKIP_UNTIL(TYPE) do { while (t->type != TYPE && t->type != 0) t++; } while (0)
        // next without (ignoring separator usually)
        #define ADVANCE_WO(WO_TYPE, token_p) do { (token_p)++; } while ((token_p)->type == WO_TYPE && (token_p)->type != 0)
        // todo: comment isn't handled -- fix now
        #define NEXT(token_p) do { (token_p)++; } while (((token_p)->type == LEX_SEPARATOR || (token_p)->type == LEX_COMMENT) && (token_p)->type != 0)
        // todo: improve error to indicate this is for funcion declaration only...
        #define DBG(TYPE) do { if (debug_print_parsing) printf("C[%c]", TYPE); } while (0)
        #define ERR(...) do { parse_error = str(&scratch, __VA_ARGS__); goto parsing_footer; } while(0)
        #define CMP(token_p, TYPE, EXACT_STR) ((token_p)->type == TYPE && eq(EXACT_STR, (token_p)->src))
        #define CONSUME(TYPE) do { NEXT(t); if (t->type != TYPE) { ERR("expected [%c] but [%c] given", TYPE, t->type); } DBG(TYPE); } while (0)
        #define CONSUME_E(TYPE, EXACT_STR) do { NEXT(t); if (!CMP(t, TYPE, EXACT_STR)) { ERR("expected [%c](\"%s\") but [%c](\"%.*s\") given", TYPE, EXACT_STR, t->type, SS(t->src)); } DBG(TYPE); } while (0)
        #define PARSE_VAR_TYPE(TYPE_DESC_OUT) do { if (debug_print_parsing) printf("(type \"%.*s\")", SS(t->src));\
                                                   if(!eq(t->src, builtin_u32.name)) ERR("unrecognized type %.*s", SS(t->src)); /*todo: should be done at another stage*/\
                                                   TYPE_DESC_OUT = &builtin_u32; } while (0)
        // WIP - look at var definition
        #define CONSUME_EXPR_END(END) do { if (debug_print_parsing) printf("(expecting expr end)");\
            do { /* (should contains only separators or comments) */\
                t++;\
                if (t->type == LEX_END_EXP) break;\
                if (t->type == LEX_SEPARATOR && eq("\n", t->src)) break;\
                if (t >= END || (t->type != LEX_COMMENT && t->type != LEX_SEPARATOR)) ERR("unexpected identifier [%c] when expression end expected", t->type);\
            } while (t->type != 0); } while (0)

        // todo: clean up var usage for number constant/literals, or it's gonna be messy
        #define CONSUMED_VAR_OR_NUMBER() do {\
                AST var = {AST_VAR};\
                var.start_t = var.end_t = t;\
                var.scope_type = scope_stack[scope_i]; /* SCOPE_EXPR */\
                var.var_type = &builtin_u32; /* todo: deduce the type from the variable/constant */\
                if (t->type == LEX_NUMBER) {\
                    printf("(a number %.*s)", SS(t->src));\
                    var.is_constant = true;\
                    var.is_function_param = false;\
                    /* push AST_VAR constant */\
                } else if (t->type == LEX_NAME) {\
                    /* could be a variable name, or a function call */\
                    var.is_constant = false;\
                    var.is_function_param = false;\
                    printf("(a variable name '%.*s')", SS(t->src)); /* todo: function calls here and stuff */\
                } else ERR("unexpected identifier [%c] in expression", t->type); /* todo: unary operators*/\
                PUSH_AST(var);\
            } while (0)

        lex_token *zt = all_tokens + all_tokens_size; // zero terminated token

        if (t->type == LEX_SEPARATOR || t->type == LEX_COMMENT) NEXT(t);
        while (t < zt) {
            printf(".");
            // todo: operator "." support, how will it look like with parsing from below?
            
            if (scope_stack[scope_i] == SCOPE_EXPR) { // we need to produce at least one AST elem ent
                // todo: paranthesis support
                // handle names or stuff
                lex_token *peek = t;
                NEXT(peek);

                if (peek->type == LEX_OPERATION) { // expression could be a serries of operation
                    printf("(operation parsing, only 2 operands WIP)");
                    // if count_operations == 1:
                    AST one_operation = {AST_OPERATION};
                    one_operation.start_t = t;
                    one_operation.scope_type = scope_stack[scope_i]; /* SCOPE_EXPR */
                    // consume operation
                    one_operation.left = all_ast + all_ast_count; // consume left operand
                    CONSUMED_VAR_OR_NUMBER();

                    CONSUME_E(LEX_OPERATION, "*"); //temp, todo: parse operation type here or later?
                    one_operation.op_type = OP_MUL;
                    NEXT(t);

                    one_operation.right = all_ast + all_ast_count; // consume right operand
                    CONSUMED_VAR_OR_NUMBER();

                    one_operation.end_t = t; // in other places we don't include end_t for var_init assuming .right will cover it, should this do the same?
                    PUSH_AST(one_operation);
                    // if count_operations > 1 - wrap inside AST_EXPRESSION with expression count...
                    peek = t; NEXT(peek); // temp for CONSUME_EXPR_END, todo: inside do{}while...
                } else { // or a single variable/constant/literal
                    CONSUMED_VAR_OR_NUMBER();
                }
                CONSUME_EXPR_END(peek);
                POP_SCOPE();
            }
            
            if (t->type == LEX_NAME) { // left hand identifier - name of function/variable when initializing/assigning to
                lex_token* t_at_name = t;
                if (eq("main", t->src) && scope_stack[scope_i] == SCOPE_FILE) {
                    printf("(main found)");
                    function_start = all_ast + all_ast_count; // tmp?
                }
                else printf("(name %.*s)", SS(t->src));

                NEXT(t);
                if (t->type == LEX_DEFINITION) {
                    printf("(definition)");
                    // consume function expecting ":: (...) = {...}"

                    // change next part to 'parse_definition which should handle () and regular types and so on'
                    // todo: more robust types, after :: it could be just a function type identifier instead of inline definition
                    NEXT(t);
                    if (CMP(t, LEX_SCOPE, "(")) { // function definition
                        printf("(func init)");
                        i32 num_params = 0;
                        do {
                            NEXT(t);
                            if (CMP(t, LEX_SCOPE, ")")) {
                                printf("(parameter end)");
                                break;
                            } else if (num_params > 0 && t->type == LEX_COMMA) {
                                NEXT(t); // consume comma
                            }
                            
                            if (t->type != LEX_NAME) ERR("expected [%c] but [%c] given", LEX_NAME, t->type);
                            // LEX_NAME consumed
                            
                            AST param = {AST_VAR_INIT};
                            param.is_function_param = true;
                            param.start_t = t;
                            printf("(function parameter \"%.*s\")", SS(t->src));
                            
                            CONSUME(LEX_DEFINITION);
                            CONSUME(LEX_NAME); /* type name */
                            type_desc* found_type_desc = 0;
                            PARSE_VAR_TYPE(found_type_desc);
                            
                            param.var_type = found_type_desc;
                            param.scope_type = SCOPE_FUNC; //scope_stack[scope_i];
                            param.end_t = t;
                            PUSH_AST(param); // this assumes we declare the function right away...
                            num_params++;
                        } while(true);
                        printf("(num params %d)", num_params);
                        // todo: definition could end here, and value assigned later, but for now we do '= { code }'
                        CONSUME_E(LEX_OPERATION, "=");
                        CONSUME_E(LEX_SCOPE, "{"); // push scope here instead??
                        PUSH_SCOPE(SCOPE_FUNC); // todo: decauple from definition? and put into another place?
                        // consume function body into all_ast -> in the next iteration
                    } else if (t->type == LEX_NAME) { // this should be a type with a regular variable definition
                        printf("(var init)");
                        type_desc* found_type_desc = 0;
                        PARSE_VAR_TYPE(found_type_desc);
                        AST var = {AST_VAR_INIT};
                        var.start_t = t_at_name; // assert
                        var.var_type = found_type_desc;
                        var.scope_type = scope_stack[scope_i];
                        var.is_function_param = false;
                        var.right = 0;
                        
                        lex_token* peek = t;
                        NEXT(peek);
                        if (CMP(peek, LEX_OPERATION, "=")) {
                            CONSUME_E(LEX_OPERATION, "="); // consume with 't'
                            PUSH_SCOPE(SCOPE_EXPR);
                            var.right = all_ast + all_ast_count; //... parse expression in next iteration, we need to check it's non zero tho (guarantee SCOPE_EXPR covers at least 1 AST)
                        } else {
                            CONSUME_EXPR_END(peek); // WIP doesn't do anything
                        }
                        var.end_t = t; // even if init has '= ...' it's not included here, but in .right AST
                        PUSH_AST(var);
                    } else ERR("expected function or variable definition, but [%c] given", t->type);
                    printf("(definition finished)");
                } else if (CMP(t, LEX_OPERATION, "=")) {
                    printf("(assign operation)");
                    AST op_assign = { AST_OPERATION };
                    op_assign.start_t = t_at_name;
                    op_assign.end_t = t;

                    op_assign.left = all_ast + all_ast_count; // filled below
                    AST var_left_hand_side = { AST_VAR };
                    var_left_hand_side.start_t = var_left_hand_side.end_t = t_at_name;
                    var_left_hand_side.scope_type = scope_stack[scope_i];
                    var_left_hand_side.var_type = &builtin_u32; /* todo: deduce the type from the variable/constant */
                    var_left_hand_side.is_constant = false;
                    var_left_hand_side.is_function_param = false;
                    PUSH_AST(var_left_hand_side);

                    op_assign.op_type = OP_ASSIGN;
                    op_assign.right = all_ast + all_ast_count; // .. prase expr in next iteration
                    PUSH_SCOPE(SCOPE_EXPR);
                    
                    PUSH_AST(op_assign);
                } else ERR("incorrect usage after using identifier name, [%c] given", t->type);
            } else if (CMP(t, LEX_SCOPE, "}")) {
                POP_SCOPE(); // anything more? check scope? currently no scoping except for function definition
            } else {
                printf("(ignored)");
                // do we dissalow random tokens, needs to be a valid expression?
            }
                
parsing_footer:
            if (debug_print_parsing) {
                printf("%c", t->type);
                if (t->type == LEX_SEPARATOR && t->src.data[0] == '\n') printf("\n");
            }
            if (parse_error.size != 0) {
                printf("\nparse error at %d:%d: %.*s\n", t->location.y, t->location.x, SS(parse_error));
                break;
            }

            NEXT(t);
            //t++;
        }
        // push dummy 0 ast at the end for convinience
        all_ast[all_ast_count] = {AST_NONE}; push_next(&glob, all_ast, sizeof(AST));

        printf("\nstep 2. parsing %s\n", parse_error.size == 0 ? "successful" : "failed");
    }

    if (parse_error.size == 0) {
        do_bytecode = true;
    }
    printf("all ast size %d\n", all_ast_count);
    // todo: typecheck? when? (should be step 3?) 
    // todo: optimization steps/passes?

    enum bk_t : u8 {
        BK_NOP,
        BK_PUSH, // it's just push stack?
            // bk_push_flag_t
        // BK_POP // pop stack
        BK_COPY,
        // BK_COPY_IMM??
        BK_MUL,
        //BK_DBG_INFO,
    };
    enum bk_push_flag_t : u8 {
        BK_PUSH_FLAG_UNINIT,
        BK_PUSH_FLAG_ZERO,
    };

    /* WIP thoughts: should we introduce registers? since if we will operate on addr only, we would have no control over
       stackable addresses and such? or do we go all in with no recursion and allocate addresses for everything statically?
    */
    struct bk {
        bk_t type;
        // ...padding, some debug info - like pointer to embeded src stuff... 
        // ... or maybe do BK_DBG which will contain more stuff, like src range and can be applied sparcefully...
        // or due to padding I can leave some generic flag1, mode1 or something tha can be used for every/some operations?
        union {
            struct {
                u32 size; // 4GB max...
                bk_push_flag_t mem_flag;
            } PUSH;
            struct {
                // using addr here is probably not the way since most of the time we will have no idea what the addr of the local var on compile time (unless static everything)
                u32 dst_addr; // 4GB va...
                u32 src_addr;
                u32 size;
            } COPY;
            struct {
                u32 op1_addr;
                u32 op2_addr;
                u32 res_addr;
            } MUL; // todo: maybe specify operation as a enum type in BK_OP or something?
        };
    };

    printf("single bk size %d\n", (int)sizeof(bk));
    struct susc_bk_header {
        u32 version = 1;
        char info[64] = "wip sus bk";
        // note: bk and data should be in continuous block... (or it will be annoying for stuff in operations specifying addr separately for 2 of those...
        u32 bk_offset;
        u32 bk_size;
        u32 data_offset;
        u32 data_size;
        //---
        u32 debug_src_offset;
        u32 debug_src_size;
    };

    //// step 3. AST to bytecode
    if (do_bytecode) {
        for (int i = 0; i < all_ast_count; i++) {
            int src_size = all_ast[i].end_t->src.data + all_ast[i].end_t->src.size - all_ast[i].start_t->src.data;
            u8* src = all_ast[i].start_t->src.data;
            printf("[%c](%.*s)\n", all_ast[i].type, src_size, src);
        }

        AST* za = all_ast + all_ast_count;
        AST* a = all_ast;
        while (a < za) {
            if (debug_print_bytecode) printf(".");

            switch (a->type) {
                case AST_NONE: ASSERT(0 && "uninitialized AST"); break;
                case AST_VAR_INIT: {
                    // BK_PUSH zeroed
                    // need to keep track var-to-addr/var-to-offset?, and it cannot be constant since this is stackable or something
                } break;
                case AST_VAR: {
                    // if constant - append to data section
                    // need to keep track which AST_VAR points to the data addr... need an array?
                    // if not constant, we don't care and propagate the addr?...
                } break;
                case AST_OPERATION: {
                    // if MUL
                    // need to BK_PUSH for the result - unless we can figure where to write immidiately,
                    // like chaining with assignment operation we can use lhs as a temp/result addr even for multiple opearions in a row
                } break;
                case AST_EXPRESSION: break; // unused, WIP
                default: ASSERT(0 && "you forgor");
            }

            a++;
        }
    }
}

// todo: bytecode runner (susc bk)

int main(int argc, char** argv) {
    if (argc < 3) {
        printf("Usage: [mode] [input file]\n");
        return 0;
    }

    buf mode = str(argv[1]);
    buf input_file_name = str(argv[2]);

    glob = arena_new(100 * 1024 * 1024); // todo: tune

    if (eq("comp", mode)) {
        printf("compile bk\n");
        comp(input_file_name);
    } else if (eq("run", mode)) {
        printf("run\n");
    } else {
        printf("[mode] '%.*s' is unknown\n", SS(mode));
    }
    // todo: comprun
    // mem stats
    printf("\nglob max mem: %f MB\n", TO_MB(glob.max_usage - glob.data));
    return 0;
}
