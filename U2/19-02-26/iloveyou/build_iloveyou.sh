#!/bin/bash

# Step 1 & 2: Create the .c file
echo ">> Creating iloveyou.c..."
cat > iloveyou.c << 'EOF'
#include <stdio.h>
#include <unistd.h>

int main() {
    for (int i = 0; i < 100; i++) {
        printf("iloveyou\n");
        fflush(stdout);
        sleep(1);
    }
    return 0;
}
EOF
echo ">> iloveyou.c created."

# Step 3: Compile with gcc
echo ">> Compiling iloveyou.c with gcc -o runFIT..."
gcc iloveyou.c -o runFIT
echo ">> Compilation done. Executable: runFIT"

# Step 4: Execute runFIT in the background
echo ">> Executing runFIT in the background..."
./runFIT &
RUN_PID=$!
echo ">> runFIT is running."

# Let it print a few times
sleep 3

# Step 5: Find the PID and kill it
echo ">> Searching for the process ID of runFIT..."
if [[ "$OSTYPE" == "msys" || "$OSTYPE" == "win32" ]]; then
    # Windows equivalent
    echo ">> Windows detected. Using taskkill..."
    taskkill /PID $RUN_PID /F
else
    # macOS / Linux
    FOUND_PID=$(pgrep -x runFIT)
    echo ">> Found PID: $FOUND_PID"
    echo ">> Killing process $FOUND_PID..."
    kill $FOUND_PID
fi

# Step 6: Done
echo ">> Finished task."
