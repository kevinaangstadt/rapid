#!/bin/bash

array=("tst" "tst2" "tst3" "tst4" "tst5" "tst6" "hamming" "string_kernels" "frequent")
for i in "${array[@]}"; do   # The quotes are necessary here
    echo "=========== $i ============="
    rapidsim="$(./rapidsim -input test/$i.txt test/$i.ap)"
    batchsim="$(./language test/$i.ap && python ./test/batchSim.py -a a.anml -s test/$i.txt)"
    echo "RAPIDSIM"
    echo "$rapidsim"
    echo "BATCHSIM"
    echo "$batchsim"
    if diff -b -w <(echo "$rapidsim") <(echo "$batchsim"); then echo "PASSED"; else echo "FAILED" ; fi
    echo "============================"
done

rm a.anml
