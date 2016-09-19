RAPID Programming Language
==========================

RAPID a high-level programming language with a combined imperative and
declarative model for programming pattern-recognition processors, such as
Micron's Automata Processor (AP). The language is targeted at identifying
"interesting" portions of a data stream. Programs define what it means for data
to be "interesting" and allow for simultaneous checking of criteria against the
data stream.

This repository hosts a prototype compiler for RAPID, which outputs a collection
of finite automata in ANML format.

RAPID is maintained by Kevin Angstadt angstadt@virginia.edu

###Associated publications

* Kevin Angstadt, Westley Weimer, and Kevin Skadron. 2016. RAPID
  Programming of Pattern-Recognition Processors. In _Proceedings of the
  Twenty-First International Conference on Architectural Support for Programming
  Languages and Operating Systems_ (ASPLOS '16). ACM, New York, NY, USA, 593-605.
  DOI: http://dx.doi.org/10.1145/2872362.2872393

##Quick Start:

To build RAPID, you will need at least version 4 the OCaml compiler (building is
successful with version 4.02.3). Support for the 'auto-tuning tessellation'
optimization requires the Micron AP SDK to be installed. This can be obtained
from www.micronautomata.com.

Run `make` from within the repository to compile the prototype RAPID compiler.

##Basic Usage:

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

##Important Language Ideas and Constructs:

Additonal details about the RAPID language may be found in Angstadt et al.'s
ASPLOS 2016 publication.  Some basic pointers are provided below.

###Networks and Macros

The `network` is the main entry point into a RAPID program. This part of a RAPID
program specifies the searches that should occur in parallel through a stream of
data.

A `macro` is used to define "interesting" portions of the input data stream.
Macros can be thought of as functions that define behavior. Driving forward
computation are comparisions against the input data stream. Access to the input
data stream is provided through a special function: `input()`. A call to
`input()` will return a character that can be used for comparision purposes.

##Current To-Do items:
- [ ] Add additional documentation on language constructs
- [ ] Update interpreter to current langauge specification
- [ ] Add in driver code to allow for offloading of RAPID programs

##Known Issues:

* Counter thresholds are all checked using the same input symbol
* Negating an OR statement currently has undefined behavior (this is also unchecked)
* Interpreter is not fully functional and does not fully match language
  specification (e.g. Whenever and Some loops not fully implemented and counter
  threshold checks are outdated)