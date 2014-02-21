#!/usr/bin/env bash

INTERFACES=("enp3s0" "enp4s0")

CMD="tshark "
for intf in ${INTERFACES[@]}; do
  CMD="$CMD -i $intf"
  echo $CMD
done

CMD="$CMD -w test.pcap"
# tshark -i eth0
echo $CMD

