Lunula
======

Lunula is a self-hosted Common Lisp to Javascript compiler.

The Plan:
=========
- Implement enough Lisp in it's own namespace (LUNULA) to be used to implement a compiler
- Make minimal use of the host Lisp
- LUNULA package will not :use COMMON-LISP, but reference required primitives explicitly.
- Implement DEFMACRO as early as possible
- Implement reader
- Implement anything else needed for compiler
- Implement Lisp to JS compiler
- Implement printer
- When Javacript code boots, copy LUNULA namespace to COMMON-LISP namespace
- Compile lunula.lisp again with itself, to validate self-hosting.

Initial Limitations:
====================
- Only simple LOOP supported
- No TAGBODY GO
- No CLOS (can be bolted on later)
- No FORMAT (yet)
- No complex numbers

Future Possibilities:
=====================
- Implement CLOS with Portable Common Loops?
- Compile to other languages, like C# or actionscript.  The goal is for compilation to other languages to be a mostly trival task.
