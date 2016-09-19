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

The file name of the output automata can be specified by a command line argument:

    $ ./rapid -o program.anml program.ap
    
If there is are parallel macros in the the network, then multiple ANML files
will be produced. Each of these will prepend an integer to the output file name.
The RAPID compiler can merge these together into a single output file:

    $ ./rapid --merge program.ap

##Important Language Ideas and Constructs:

Additional details about the RAPID language may be found in Angstadt et al.'s
ASPLOS 2016 publication.  Some basic pointers are provided below.

###Networks and Macros

The `network` is the main entry point into a RAPID program. This part of a RAPID
program specifies the searches that should occur in parallel through a stream of
data.

A `macro` is used to define "interesting" portions of the input data stream.
Macros can be thought of as functions that define behavior. Driving forward
computation are comparisons against the input data stream.

###Input Data Stream

Access to the input data stream is provided through a special function:
`input()`. A call to `input()` will return a character that can be used for
comparison purposes. Characters are 8-bit and may be written using hex notation
(e.g. 0x40 for '@').

####Reserved Characters

RAPID supports data streams containing hex characters 0x00 -- 0xFD. If your data
also contains 0xFE or 0xFF characters, you may experience abnormal behavior.

Hex code 0xFE is reserved for triggering counter comparisons. Currently, you
must inject these characters into the data stream at the correct locations. In
the future, this will be handled by a library.

Hex code 0xFF is reserved for START_OF_DATA in RAPID. This is used to trigger
the start of matching within the Network. You may use this in your data stream
to trigger the beginning of pattern matching.

####Special Characters

You may use `ALL_INPUT == input()` to represent matching on any character being
read from the input stream and `START_OF_INPUT == input()` to represent matching
the start of data.

###Reporting

Use the `report` statement to trigger a reporting event. This will capture the
current offset in the input data stream and the current macro allowing for
"interesting" data to be post-processed.

###Either/Orelse Statement

To instantiate a static number of parallel checks against the input stream, use
an either/orelese statement. Each block in this statement is executed in
parallel.

For example, to search for both 'and' and 'or' in parallel, use:

    either {
      'a' == input();
      'n' == input();
      'd' == input();
    } orelse {
      'o' == input();
      'r' == input();
    }

###Some Statement

Suppose you have an array of strings and would like to perform parallel matches
against the input stream using computation that includes these strings. This can
be achieved using a `some` statement. The syntax is similar to a for-in loop in
Java and instantiates a parallel computation for each element provided.

For example, to search for all strings within a Hamming distance of 5 from any
string in the `dna` array, the following would work (assuming you have written a
Hamming distance macro):

    some(String s : dna) {
        hamming_distance(s,5);
    }

###Whenever Statement

RAPID also has built-in support for sliding window searches. This allows for a
concise means of specifying where to begin pattern matching within the input
stream. For example, you may begin searching only at the start of data or after
seeing a particular data sequence.

As an example, one could match the word "rapid" anywhere in the data stream by
performing a sliding window search starting with every input character as
follows:

    whenever(  ALL_INPUT == input() ) {
        foreach(char c : "rapid")
            c == input();
        report;
    }

###Auto-Tuning Tessellation
When compiling for a spatial architecture like the Automata Processor,
producing the smallest (in terms of hardware resources) design possible is
important. This allows for more widgets or macros to be loaded and executed in
parallel on the device. Placement and routing algorithms, however, become less
efficient as more hardware resources are used.

We have developed an auto-tuning tessellation technique to help balance these
two factors when developing for the AP. We leverage the AP runtime's ability to
load several designs and execute in parallel. Loading occurs on a block level (a
block is a subregion of the AP's spatial layout). The RAPID compiler can extract
repeated portions of a program (for example when the same macro is applied to an
array of strings) and compile the design once. At runtime, this design can be
loaded repeatedly to support all of the strings in the array.

An extracted design, however, might not use all of the resources within a given
block or collection of blocks. To mitigate this, the compiler can try to use
these resources with additional copies of the design.

To use the tiling optimization, pass in the `--tiling` flag to the compiler:

    $ ./rapid --tiling program.ap

The compiler will output a mapping of array offsets to STE (state) identifiers
which can be used for symbol replacement. In the future, symbol replacement will
be handled by a runtime library.

##Current To-Do items:
- [ ] Add additional documentation on language constructs
- [ ] Update interpreter to current language specification
- [ ] Add in library code to allow for offloading of RAPID programs

##Known Issues:

* Counter thresholds are all checked using the same input symbol
* Negating an OR statement currently has undefined behavior (this is also unchecked)
* Interpreter is not fully functional and does not fully match language
  specification (e.g. Whenever and Some loops not fully implemented and counter
  threshold checks are outdated)