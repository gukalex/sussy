/*
sus lang compiler
compile windows (user lib probably not needed):
time clang sussy.cpp -std=c++17 -g -luser32.lib -Wno-deprecated-declarations -o sussy.exe
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


// TYPE ALIASES START
using u8 = unsigned char;
using i8 = char;
using u32 = unsigned int;
using u64 = unsigned long long int;
using i32 = int;
using i64 = unsigned long long int;

// COMMON STRUCTS AND FUNCTIONS START
struct buf {
    u8* data;
    u64 size;
};
// sized string macro, use as printf("as string %.*s", SS(my_buf))
#define SS(b) (int)b.size, b.data

struct arena {
    u8* data;
    u8* curr;
    u64 size;
} glob;

u8* push(arena* a, u64 size) { // todo: alignment
    u8* out = a->curr;
    a->curr += size;
    // todo: assert if OOM
    return out;
}

buf str(const char* cstr) { // buf view
    return {(u8*)cstr, strlen(cstr)};
}

buf strc(const char* cstr) { // creates a buf with a copy to glob
    u64 size = strlen(cstr);
    buf out = { push(&glob, size + 1), size };
    memcpy(glob.curr, cstr, size + 1); // keeping 0 term but not reflecting it in 'size'
    return {(u8*)glob.curr, size};
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

buf read_file(buf input_file_name, int zero_padding_bytes = 1) {
    buf f = {};
	FILE* file = fopen((const char*)input_file_name.data, "rb");
	if (file != NULL) {
		fseek(file, 0, SEEK_END);
		f.size = ftell(file);
		fseek(file, 0, SEEK_SET);

		f.data = push(&glob, f.size + zero_padding_bytes); // todo: assert temp size

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

// COMPILER START

bool idt_start(char c) { // is start of identifier, cannot include numbers or other symbols except '_'
    if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_')
        return true;
    return false;
}

enum token {
    NONE, // empty
    NAME, // identifier name, function/variable
    COMMENT,
    SEPARATOR, // spaces, new lines
    END_EXP, // ';'
};

struct token_mtd {
    buf src;
    token t;
};

token_mtd consume(char** addr_for_c, token t) {
    char *c = *addr_for_c;
    token_mtd mtd = { buf{(u8*)c, 0}, t };
    switch (t) {
        case COMMENT:
        // c++
        // mtd.src = {c, diff}
        break;
        case NAME:
        //...
        default: 
        break;
    }
    return mtd;
}

void comp(buf input_file_name) {
    // read the file
    // todo: assert utf8/ascii encoding or something
    buf input = read_file(input_file_name, 10); /* we can read a few bytes past the end of the file and be fine */
    printf("input:\n %.*s\n", SS(input));
    printf("input size: %d\n", (int)input.size);
    
    // an array of identifiers and their types
    struct {
        buf name;
        buf type;
    } idents[1000] = {}; // todo: proper sizing
    int idents_count = 0;

    // parse toplevel - get a list of identifiers
    // todo: tokens
    char *c = (i8*)input.data;
    while ((u8*)c < input.data + input.size) {
        int curr = (u64)c - (u64)input.data;
        //printf("%d: %c (%d)\n", curr, (*c == '\n' ? '^' : *c), *c);
        switch (*c) {
            case ' ': case '\n': case '\r': case '\t':
            c++; break;
            case '/':
                if (c[1] == '/') { // 1. can be a // comment
                    while(*c != '\n' && *c != 0) c++;
                    break;
                }
                if (c[1] == '*') { // 2. can be a /* */ comment
                    // todo: nested comments
                    c += 2; // revind to the char past '*'
                    while (!(c[0] == '*' && c[1] == '/') && *c != 0) c++;
                    c += 2; // skip past the last '/'
                    break;
                }
                c++; break; // 3. can be a division in an expression
            default:
                if (idt_start(*c)) { 
                    //... = consume(&c, NAME);
                }
                c++; break;
        }
        // process token for next stage -> add to token list
    }
}

int main(int argc, char** argv) {
    if (argc < 3) {
        printf("Usage: [mode] [input file]\n");
        return 0;
    }

    buf mode = str(argv[1]);
    buf input_file_name = str(argv[2]);

    glob.size = 100 * 1024 * 1024;
    glob.data = glob.curr = (u8*)calloc(1, glob.size);

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
