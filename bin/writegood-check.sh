#!/usr/bin/env bash
# writegood-check.sh by Amory Meltzer
# Bulk-check writing
# Should also run nom's/node's writegood

echo "Weasel words:"
writegood--weaselwords.sh $1
echo
echo "Passive voice:"
writegood--passivevoice.sh $1
echo
echo "Lexical dups:"
writegood--lexicalillusions.pl $1
echo
echo "Proselint:"
proselint $1
echo
