#!/bin/bash

array=("tst" "tst2" "tst3" "tst4" "tst5" "tst6" "hamming" "string_kernels" "frequent")
output=""
for i in "${array[@]}"; do   # The quotes are necessary here
    echo "=========== $i ============="
    rapidsim="$(./rapidsim -input test/$i.txt test/$i.ap)"
    batchsim="$(./language test/$i.ap && python ./test/batchSim.py -a 0.a.anml -s test/$i.txt)"
    echo "RAPIDSIM"
    echo "$rapidsim"
    echo "BATCHSIM"
    echo "$batchsim"
    if diff -b -w <(echo "$rapidsim") <(echo "$batchsim"); then output="$output $i -- PASSED\n" && echo "PASSED"; else output="$output $i -- FAILED\n" && echo "FAILED" ; fi
    echo "============================"
done
echo -e "$output"
rm 0.a.anml
