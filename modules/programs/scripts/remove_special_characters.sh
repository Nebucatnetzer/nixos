#!/usr/bin/env bash
find . -print | while read file; do
    file_clean=${file//[ ()&\'\,]/_}
    mv "$file" "$file_clean"
done
