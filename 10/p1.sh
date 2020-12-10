paste <(sort -n input.txt | head -n -1) <(sort -n input.txt | tail -n +2) | awk '{print $2-$1}' | sort | uniq -c | awk 'BEGIN {product = 1}{ product*= $1 + 1} END {print product}'


