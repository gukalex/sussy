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

// TYPE ALIASES START
using u8 = unsigned char;
using i8 = char;
using u32 = unsigned int;
using u64 = unsigned long long int;
using i32 = int;
using i64 = unsigned long long int;

// COMMON STRUCTS AND FUNCTIONS START

//printf(__VA_ARGS__);
#define ASSERT(cond) do { if (!(cond)) { printf("Assert at %s:%d (%s) - %s\n",__FILE__, __LINE__, __func__, #cond); fflush(stdout); *(int*)(0) = 0; } } while (0)
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
    // u64 counter; // some stuff for stats and keeping track for valid use??
} glob;

u8* push(arena* a, u64 size) { // todo: alignment
    ASSERT(a->locked_by == 0); // use push_next if arena in push mode
    u8* out = a->curr;
    a->curr += size;
    ASSERT(a->curr <= a->data + a->size); // todo: grow alloc
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
    a->locked_by = 0;
}
// todo: pop_to

arena arena_new(u64 size) {
    arena a = {};
    a.size = size;
    a.data = a.curr = (u8*)calloc(1, a.size);
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

//todo: str as snprintf with temp/custom allocator

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

// COMPILER START

// values just for debugging, also some stuff could be condensed into larger groups like punctuation, separators, operations
// todo: rename lex_token_t
enum token_t {
    UNKNOWN_ZERO = 0, // ?? idk zero initilization good ??
    UNKNOWN = '?',    // empty
    COMMA = ',', // could be a separator?
    NAME = 'n', // identifier name, function/variable, keyword??
    COMMENT = 'c', // comment '//' or '/* */'
    SEPARATOR = 's', // spaces, tabs, '\r', new lines
    END_EXP = ';', // ';'
    DEFINITION = ':', // :: definition
    OPERATION = 'O', // =, *, -, +, \, %, !, ~, &&, ||, &, |, ==, <, >, <=, >=, shifts << >> (not handling <<=, >>=)
    SCOPE = '^', // {}, (), []
    NUMBER = '1', // number constants
    // todo: char, string, raw string, '? ternary', -> ' <-
};

struct token { // todo: rename lex_token
    buf src;
    token_t type;
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

void comp(buf input_file_name) {
    // todo: assert utf8/ascii encoding or something
    buf input = read_file(input_file_name, 10, &glob); // we can read a few bytes past the end of the file and be fine
    printf("input:\n %.*s\n", SS(input));
    printf("input size: %d\n", (int)input.size);

    arena scratch = arena_new(64 * 1024); // a small thing, just for errors?
    defer { arena_delete(&scratch); };

    buf error = {}; // todo: add a proper error type enum ?
    u8 *c = input.data;
    i32 lines_total = 1; // need to keep a line number for error msg and other stuff
    u8* line_start = c;

    token* all_tokens = (token*)push_start(&glob);
    u32 curr_t = 0;

    while (c < input.data + input.size) {
        token t = {{c, 0}, UNKNOWN};
        i32 token_new_lines = 0;
        #define NEWLINE() { c++; token_new_lines++; line_start = c; }

        switch (*c) {
            case ',': t.type = COMMA; c++; break;
            case '\n': t.type = SEPARATOR; NEWLINE(); break;
            case ' ': case '\r': case '\t': t.type = SEPARATOR; c++; break;
            case ';': t.type = END_EXP; c++; break;
            case '{': case '}': case '(': case ')': case '[': case ']': t.type = SCOPE; c++; break;
            case '*': case '+': case '-': case '%': case '!': case '~': {
                t.type = OPERATION;
                c++; 
                if (*c == '=') c++;  // +=, -=, != except >=, <= which handled below
            } break;
            // =, ==, &, &&, |, ||, >, <, >=, <=, <<, >> handle with pairs, no <<=, >>= because I never use those
            case '=': case '&': case '|': case '>': case '<':
                t.type = OPERATION;
                if (c[1] == *c || c[1] == '=') c += 2; // pairs ==, &&, << ... and <=, >=, &=, |=
                else c++;
                break;
            case '/': { // comment or division operation
                if (c[1] == '/') { // 1. can be a // comment
                    t.type = COMMENT;
                    while(*c != '\n' && *c != 0) c++;
                    NEWLINE(); // consume new line
                    break;
                }
                if (c[1] == '*') { // 2. can be a /* */ comment
                    t.type = COMMENT;
                    // todo: nested comments !!
                    c += 2; // revind to the char past '*'
                    while (!(c[0] == '*' && c[1] == '/') && *c != 0) {
                        if (*c == '\n') { NEWLINE(); }
                        else c++;
                    }
                    c += 2; // skip past the last '/'
                    break;
                }
                t.type = OPERATION; // 3. can be a division in an expression
                c++;
                if (*c == '=') c++; // /=
            } break;
            case ':': {
                if (c[1] == ':') {
                    t.type = DEFINITION;
                    c++;
                } else {
                    error = str("single ':', but '::' expected");
                }
                c++;
            } break;
            default: {
                if (IDT_1ST(*c)) { // consume identifier name, '6i' works for now where 6 is ignored
                    t.type = NAME;
                    while(IDT_2ND(*c)) c++;
                    break;
                }
                if (NUM_1ST(*c)) {
                    t.type = NUMBER;
                    while (NUM_2ND(*c)) c++;
                    break;
                }
                error = str(&scratch, "unknown char [%c]", *c);
                c++; 
            } break;
        }

        u32* stuff = (u32*)push(&glob, 10);
        pop_to(&glob, stuff);

        lines_total += token_new_lines;

        t.src.size = c - t.src.data;
        t.new_lines = token_new_lines;
        t.location.x = t.src.data - line_start + 1; // +1 to start from 1 
        t.location.y = lines_total;
        // todo: process token for next stage -> add to token list
        
        all_tokens[curr_t] = t;
        push_next(&glob, all_tokens, sizeof(token));
        curr_t++;

        printf("[%c]%.*s", t.type, SS(t.src));
        if (error.size != 0) {
            printf("\nsyntax error at %d:%d: %.*s\n", t.location.y, t.location.x, SS(error));
            // todo: set some error state to return/signal
            break; // todo: some stuff can be ignored or that's a bad idea?
        }
    }
    printf("\nStep 1. parsing %s\n", error.size == 0 ? "successful" : "failed");
    push_end(&glob, all_tokens);

    printf("token count %d\n", curr_t);
}

int main(int argc, char** argv) {
    if (argc < 3) {
        printf("Usage: [mode] [input file]\n");
        return 0;
    }

    buf mode = str(argv[1]);
    buf input_file_name = str(argv[2]);

    glob = arena_new(100 * 1024 * 1024); // todo: tune

    if (eq("comp", mode)) {
        printf("compile/type check\n");
        comp(input_file_name);
    } else if (eq("run", mode)) {
        printf("run\n");
    } else {
        printf("[mode] '%.*s' is unknown\n", SS(mode));
    }
    // todo: comprun

    return 0;
}
