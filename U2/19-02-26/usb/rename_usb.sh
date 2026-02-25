#!/bin/bash

# Step 1: Find the USB drive
echo ">> Searching for USB drives in /Volumes..."
USB_PATH=""

for volume in /Volumes/*; do
    # Skip the main system disk
    if [[ "$volume" != "/Volumes/Macintosh HD" && "$volume" != "/Volumes/$(diskutil info / | grep 'Volume Name' | awk '{print $3}')" ]]; then
        USB_PATH="$volume"
        break
    fi
done

if [[ -z "$USB_PATH" ]]; then
    echo ">> No USB drive found. Make sure it is plugged in."
    exit 1
fi

echo ">> Found USB drive: $USB_PATH"

# Step 2: Rename all files by adding "." before each name
echo ">> Renaming all files (adding '.' prefix)..."

find "$USB_PATH" -maxdepth 1 -type f | while read -r filepath; do
    dir=$(dirname "$filepath")
    filename=$(basename "$filepath")

    # Skip files that already start with "."
    if [[ "$filename" == .* ]]; then
        echo ">> Skipping (already hidden): $filename"
        continue
    fi

    mv "$filepath" "$dir/.$filename"
    echo ">> Renamed: $filename -> .$filename"
done

echo ">> Finished task."
