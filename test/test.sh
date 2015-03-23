#!/bin/sh

array=("tst" "string_kernels" "frequent")
for i in "${array[@]}"; do   # The quotes are necessary here
    echo "=========== $i ============="
    ../rapidsim -input $i.txt $i.ap
    echo "============================"
done
