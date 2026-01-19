#!/bin/bash
# Uploads binary files named
#   vds21.bin, vds22.bin, vds23.bin, ..., vds30.bin
# to devices with IP addresses:
#   192.168.1.121, 192.168.1.122, 192.168.1.123, ..., 192.168.1.130
# ...respectively.
for i in `seq 21 30`; do
    printf "$i - "; curl -F "image=@vds${i}.bin" 192.168.1.$(expr 100 + $i)/update; printf "\n"
done
