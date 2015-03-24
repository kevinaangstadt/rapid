#!/bin/sh

array=("tst" "tst2" "string_kernels" "frequent")
for i in "${array[@]}"; do   # The quotes are necessary here
    echo "=========== $i ============="
    ./rapidsim -input test/$i.txt test/$i.ap
    echo "============================"
done
