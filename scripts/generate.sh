#!/bin/sh

#
# This script can be used to regenerate the bindings.lisp file using
# SWIG. Should not be necessary to do that, though, unless there's a
# new version out.
#

swig -cffi -module bindings -noswig-lisp -o bindings.lisp scripts/bindings.i 

sed -i 's|( 127)|(- 127)|' bindings.lisp
sed -i 's|(cl:- "2.0.20" "stable")|"2.0.20-stable"|' bindings.lisp
