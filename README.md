# Overview

This package provides Emacs Grand Unified Debugger support for the lldb
debugger.  It is based on parsing the text output of the lldb executable on the
console.

The mode provides a minimum on functionality, including showing the code while
stepping through the program, setting and removing breakpoints from code
buffers, traversing the callstack, and evaluating expressions around the point
in a code buffer.

It has been tested mostly on various versions of macOS, with lldb executables
that are included with Apple's Xcode.
