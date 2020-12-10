expr $(expr 1 + $(paste <(sort -n input.txt | head -n -1) <(sort -n input.txt | tail -n +2) | awk '{print $2-$1}' | grep "1" | wc -l))  \
\* $(expr 1 + $(paste <(sort -n input.txt | head -n -1) <(sort -n input.txt | tail -n +2) | awk '{print $2-$1}' | grep "3" | wc -l ))


