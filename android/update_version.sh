#!/bin/bash

FILE=AndroidManifest.xml
new_version=$(git log --oneline | wc -l)

echo "new version = $new_version"
if [ -f $FILE ] ; then
    sed_script="s/\(android:versionCode[^=]*=[^\"]*\"\)[^\"]*\"/\1$new_version\"/"
    sed -i $sed_script $FILE
else
    echo "Error: there is no AndroidManifest.xml file"
fi
