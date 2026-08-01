# wrapper.pyx
# cdef extern from "ada_library.h":
cdef extern from *:
    """
    // This raw C code is injected directly into the generated .c file.
    // It acts as a header-less declaration for the compiler.
    int ada_add(int a, int b);
    void ada_message(void);
    """
    # Declare the functions exactly as they appear in your C-style header
    int ada_add(int a, int b)
    void ada_message()

def py_ada_add(int a, int b):
    """A Python-accessible wrapper for the Ada 'add' function."""
    return ada_add(a, b)

def py_ada_message():
    """Triggers the Ada hello world procedure."""
    ada_message()

