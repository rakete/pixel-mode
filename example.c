#include "stdint.h"

// palette: example_palette
// format: rgb
uint8_t palette[1*9*3] = {
    0,   0,   0,
    255, 255, 255,
    0,   0,   0,
    255,   0,   0,
    0, 255,   0,
    0,   0, 255,
    255, 255,   0,
    0, 255, 255,
    230,   0, 150
};

// bitmap: example_A
// using: example_palette
static int32_t pixels[6*7] = {
    0, 0, 0, 0, 0, 0,
    0, 0, 1, 1, 0, 0,
    0, 1, 0, 0, 1, 0,
    0, 1, 1, 1, 1, 0,
    0, 1, 0, 0, 1, 0,
    0, 1, 0, 0, 1, 0,
    0, 0, 0, 0, 0, 0
};

int main() {
    return 0;
}
