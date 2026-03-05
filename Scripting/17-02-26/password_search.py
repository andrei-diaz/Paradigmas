#!/usr/bin/env python3
"""
Password Search Script
Searches through a password dataset line by line until it finds the target password.
"""

import sys
import time


TARGET_PASSWORD = "dragon"


def search_password(dataset_path):
    print(f"[*] Loading dataset: {dataset_path}")
    print(f"[*] Searching for target password...\n")

    start_time = time.time()
    attempts = 0

    try:
        with open(dataset_path, "r", encoding="utf-8", errors="ignore") as f:
            for line in f:
                password = line.strip()
                attempts += 1

                if attempts % 500_000 == 0:
                    print(f"    [{attempts:,} attempts so far...]")

                if password == TARGET_PASSWORD:
                    elapsed = time.time() - start_time
                    print(f"\n[+] PASSWORD FOUND!")
                    print(f"[+] Password : {password}")
                    print(f"[+] Attempts : {attempts:,}")
                    print(f"[+] Time     : {elapsed:.4f} seconds")
                    return

        elapsed = time.time() - start_time
        print(f"\n[-] Password not found in dataset.")
        print(f"[-] Attempts : {attempts:,}")
        print(f"[-] Time     : {elapsed:.4f} seconds")

    except FileNotFoundError:
        print(f"[!] Error: File '{dataset_path}' not found.")
        sys.exit(1)


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"Usage: python {sys.argv[0]} <dataset_path>")
        sys.exit(1)

    search_password(sys.argv[1])
