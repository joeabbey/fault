#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "sample.h"
#include "config.h"

#define MAX_SIZE 1024
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define VERSION "1.0.0"

/* A block comment
 * spanning multiple lines
 * struct FakeStruct {} should not be parsed
 */

typedef unsigned long ulong;

struct node {
    int value;
    struct node* next;
};

enum direction {
    NORTH,
    SOUTH,
    EAST,
    WEST
};

// Public function
int add(int a, int b) {
    return a + b;
}

void print_point(const Point* p) {
    printf("(%d, %d)\n", p->x, p->y);
}

char* format_name(const char* first, const char* last) {
    char* buf = malloc(128);
    snprintf(buf, 128, "%s %s", first, last);
    return buf;
}

// Static (internal) function
static int internal_helper(void) {
    return 42;
}

static void another_static(int x) {
    printf("%d\n", x);
}

int main(int argc, char** argv) {
    Point p = {1, 2};
    print_point(&p);
    return 0;
}
