import os
import glob
import subprocess

# Get all files starting with main_
files = glob.glob("main_*.py")

# Sort for consistent execution order
files.sort()

for file in files:
    print(f"Running {file}...")
    subprocess.run(["python", file], check=True)

print("All main_ scripts completed.")