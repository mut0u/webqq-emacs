#!/usr/bin/env bash
# author savior

PD=$1;
HEX_CODE=$2;
V_CODE=$3;
STEP_1=`echo -n $PD | md5sum | awk '{print $1}'`
#POSTFIX=`echo ${HEX_CODE//\\\x/}`;
STEP_2=`echo -n $STEP_1$HEX_CODE | xxd -r -p | md5sum | awk '{print $1}'`
T=$STEP_2$V_CODE
STEP_3=`echo -n ${T^^} | md5sum | awk '{print $1}'`
echo -n $STEP_3
