You write (or work on) large, modern fortran code bases.  These
make heavy use of function overloading and generic interfaces.  Your
brain is too small to remember what all the specialisers are
called.  Therefore, your editor should help you.

Load this file and tell it to parse all the fortran files in your
code base.  You can do this one directory at a time by calling
`f90-parse-interfaces-in-dir' (M-x f90-parse-interfaces-in-dir
RET).  Or you can parse all the fortran files in a directory and
recursively in its subdirectories by calling
`f90-parse-all-interfaces'.

Now you are able to browse (with completion) all defined interfaces
in your code by calling `f90-browse-interface-specialisers'.
Alternatively, if `point' is on a procedure call, you can call
`f90-find-tag-interface' and you'll be shown a list of the
interfaces that match the (possibly typed) argument list of the
current procedure.  This latter hooks into the `find-tag' machinery
so that you can use it on the M-.  keybinding and it will fall back
to completing tag names if you don't want to look for an interface
definition.
In addition, if you're in a large procedure and want the list of
the variables in scope (perhaps you want to define a new loop
variable), you can use `f90-list-in-scope-vars' to pop up a buffer
giving a reasonable guess.  Note this doesn't give you module
variables, or the variables of parent procedures if the current
subroutine is contained within another.

Derived types are also parsed, so that slot types of derived types
are given the correct type (rather than a UNION-TYPE) when arglist
matching.  You can show the definition of a known derived type by
calling `f90-show-type-definition' which prompts (with completion)
for a typename to show.

The parser assumes you write Fortran in the style espoused in
Metcalf, Reid and Cohen.  Particularly, variable declarations use a
double colon to separate the type from the name list.

Here's an example of a derived type definition
    type foo
       real, allocatable, dimension(:) :: a
       integer, pointer :: b, c(:)
       type(bar) :: d
    end type

Here's a subroutine declaration
    subroutine foo(a, b)
       integer, intent(in) :: a
       real, intent(inout), dimension(:,:) :: b
       ...
    end subroutine foo

Local procedures whose names conflict with global ones will likely
confuse the parser.  For example

   subroutine foo(a, b)
      ...
   end subroutine foo

   subroutine bar(a, b)
      ...
      call subroutine foo
      ...
    contains
      subroutine foo
         ...
      end subroutine foo
   end subroutine bar

Also not handled are overloaded operators, scalar precision
modifiers, like integer(kind=c_int), for which the precision is
just ignored, and many other aspects.

Some tests of the parser are available in f90-tests.el (in the same
repository as this file).
