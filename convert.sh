#!/bin/bash

for file in outfile*.gv.txt; do
    base="${file%.gv.txt}"
    dot -Tsvg "$file" > "$base.svg"
done

dot -Tsvg result.gv.txt > result.svg
