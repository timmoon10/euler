#/bin/sh
cat p022_names.txt | sed 's/"//g' | sed 's/,/\n/g' | sort | ./score.awk
