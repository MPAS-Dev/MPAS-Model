#!/bin/sh
#
# Create module interface statements from GSW .f90 files.
#
# Usage: ./make_mod.sh *.f90
#

toolbox_name="gsw_mod_toolbox"

outfile=${toolbox_name}".f90"

tmpfile="__temp.$$"

echo "module "${toolbox_name} > $outfile
cat << END >> $outfile

use gsw_mod_kinds

implicit none

END

while [ -n "$1" ]; do

	mod=`grep "module " $1`
	if [ -n "$mod" ]; then
		echo "ignoring module: "$1
		shift
		continue
	fi
	mod=`grep "elemental " $1`
	if [ -z "$mod" ]; then
		mod=`grep "pure " $1`
		if [ -z "$mod" ]; then
			echo "ignoring non-elemental/pure routine: "$1
			shift
			continue
		fi
	fi

	echo "public :: "`basename $1 .f90` >> $outfile

	awk '
/ *function *(.*) *result(.*)/ {
	i = index($0,"result(");
	var = substr($0,i+7);
	i = index(var,")");
	var = ":: *"substr(var,0,i-1)
}
length(var) > 0 && $0 ~ var {
	print $0 "!intent(result)";
	next
}
/^contains/ {
	incon = 1;
	next;
}
/^end fun/ {
	incon = 0;
}
/^end sub/ {
	incon = 0;
}
incon == 1 {
	next;
}
	{
	print $0;
}' $1 | sed -n '
/^elemental sub.*& *$/ {
	N
	p
	s/^.*\(gsw_[a-z0-9_]*\) *(.*/\1/
	h
	b
}
/^elemental func.*& *$/ {
	N
	p
	s/^.*\(gsw_[a-z0-9_]*\) *(.*/\1/
	h
	b
}
/^pure sub.*& *$/ {
	N
	p
	s/^.*\(gsw_[a-z0-9_]*\) *(.*/\1/
	h
	b
}
/^pure func.*& *$/ {
	N
	p
	s/^.*\(gsw_[a-z0-9_]*\) *(.*/\1/
	h
	b
}
/^elemental func/ {
	p
	s/^.*\(gsw_[a-z0-9_]*\) *(.*/\1/
	h
	b
}
/^elemental sub/ {
	p
	s/^.*\(gsw_[a-z0-9_]*\) *(.*/\1/
	h
	b
}
/^pure func/ {
	p
	s/^.*\(gsw_[a-z0-9_]*\) *(.*/\1/
	h
	b
}
/^pure sub/ {
	p
	s/^.*\(gsw_[a-z0-9_]*\) *(.*/\1/
	h
	b
}
/:: *gsw_.*/ {
	p
	b
}
/^end func/ {
	G
	s/\n/ /
	p
	b
}
/^end sub/ {
	G
	s/\n/ /
	p
	b
}
/mod_kinds/ { p }
/implicit / { p }
/intent(in)/ { p }
/intent(out)/ { p }
/intent(result)/ {
	s/\(.*\)!intent.*/\1/
	p
}
' >> $tmpfile

	echo "" >> $tmpfile
	shift
done

cat << END >> $outfile

interface

END

sed 's/\(.*\)/    \1/' $tmpfile >> $outfile
/bin/rm $tmpfile

cat << END >> $outfile
end interface

END
echo "end module "${toolbox_name} >> $outfile
