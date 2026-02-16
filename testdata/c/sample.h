#ifndef SAMPLE_H
#define SAMPLE_H

#include <stdio.h>
#include <stdlib.h>
#include "utils.h"

// Type definitions
typedef unsigned int uint32;
typedef struct point Point;

// Struct definitions
struct point {
    int x;
    int y;
};

typedef struct {
    char name[64];
    int age;
} Person;

// Enum definition
enum color {
    RED,
    GREEN,
    BLUE
};

typedef enum {
    STATUS_OK,
    STATUS_ERROR,
    STATUS_PENDING
} Status;

// Function declarations
int add(int a, int b);
void print_point(const Point* p);
char* format_name(const char* first, const char* last);
static int internal_helper(void);

#endif /* SAMPLE_H */
