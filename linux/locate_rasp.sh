#!/bin/bash

echo "searching available servers ..."

machines=""

for i in $(seq 1 6) ; do
    ping -i 0.2  -c 1 192.168.1.$i 2>&1 1>/dev/null
    if [ "$?" = "0" ] ; then
        machines="$machines 192.168.1.$i"
    fi
done

echo "testing $machines ..."

options="-o PasswordAuthentication=no -o StrictHostKeyChecking=no"
for i in $machines ; do
    name=$(ssh $options pi@$i hostname 2>/dev/null)
    if [ "$name" = "raspberrypi" ] ; then
        echo " raspberry = $i"
    fi
done

echo "end"
