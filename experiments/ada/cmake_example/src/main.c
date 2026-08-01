#include <stdio.h>

// Prototypes for Ada runtime init/final (provided by GNAT)
extern void adainit(void);
extern void adafinal(void);

// The exported Ada function
extern void ada_message(void);

int main() {
    adainit();   // Initialize Ada environment
    ada_message();
    adafinal();  // Clean up Ada environment
    return 0;
}
