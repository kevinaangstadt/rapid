#!/bin/sh

array=("tst" "tst2" "tst3" "tst4" "tst5" "tst6" "hamming" "string_kernels" "frequent")
for i in "${array[@]}"; do   # The quotes are necessary here
    echo "=========== $i ============="
    ./rapidsim -input test/$i.txt test/$i.ap
    echo "============================"
done
