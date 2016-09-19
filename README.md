RAPID Programming Language
==========================

RAPID a high-level programming language with a combined imperative and
declarative model for programming pattern-recognition processors, such as
Micron's Automata Processor (AP).

This repository hosts a prototype compiler for RAPID, which outputs a collection
of finite automata in ANML format.

RAPID is maintained by Kevin Angstadt angstadt@virginia.edu

### Associated publications

* Kevin Angstadt, Westley Weimer, and Kevin Skadron. 2016. RAPID
  Programming of Pattern-Recognition Processors. In _Proceedings of the
  Twenty-First International Conference on Architectural Support for Programming
  Languages and Operating Systems_ (ASPLOS '16). ACM, New York, NY, USA, 593-605.
  DOI: http://dx.doi.org/10.1145/2872362.2872393

Quick Start:
____________

To build RAPID, you will need at least version 4 the OCaml compiler (building is
successful with version 4.02.3). Support for the 'auto-tuning tessellation'
optimization requires the Micron AP SDK to be installed. This can be obtained
from www.micronautomata.com.

Run `make` from within the repository to compile the prototype RAPID compiler.

Basic Usage:
____________

To compile a RAPID program, simply pass it to the RAPID compiler:

    $ ./rapid program.ap
    
This will produce **a.anml** (or **#.a.anml** if there are multiple parallel
macros in the network)

The filename of the output automata can be specified by a command line argument:

    $ ./rapid -o program.anml program.ap
    
If there is are parallel macros in the the network, then multiple ANML files
will be produced. Each of these will prepend an integer to the output filename.
The RAPID compiler can merge these together into a single output file:

    $ ./rapid --merge program.ap

Known Issues:
_____________
* Counter thresholds are all checked using the same input symbol
* Negating an OR statement currently has undefined behavior (this is also unchecked)
* Interpreter is not fully functional and does not fully match language
  specification (e.g. Whenever and Some loops not fully implemented and counter
  threshold checks are outdated)