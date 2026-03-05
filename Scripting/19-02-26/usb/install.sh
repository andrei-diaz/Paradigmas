#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SCRIPT_PATH="$SCRIPT_DIR/rename_usb.sh"
PLIST_PATH="$HOME/Library/LaunchAgents/com.user.usbrenamer.plist"

chmod +x "$SCRIPT_PATH"

echo ">> Installing LaunchAgent..."

cat > "$PLIST_PATH" << EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>com.user.usbrenamer</string>
    <key>ProgramArguments</key>
    <array>
        <string>/bin/bash</string>
        <string>$SCRIPT_PATH</string>
    </array>
    <key>WatchPaths</key>
    <array>
        <string>/Volumes</string>
    </array>
    <key>RunAtLoad</key>
    <false/>
</dict>
</plist>
EOF

launchctl load "$PLIST_PATH"
echo ">> LaunchAgent loaded. rename_usb.sh will now run automatically on USB insert."
echo ">> To uninstall, run: launchctl unload $PLIST_PATH"
