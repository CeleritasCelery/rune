#!/opt/homebrew/bin/python3

import os

# incramentally run check.js and cargo run each with the same number of steps.
# compare the output of js_output.txt and rust_output.txt to see if they are the
# same.

file = "../crdt-testdata/data/sveltecomponent.json"
for i in range(0, 18000, 10):
# i = 92
    print(f"Running with {i} steps")
    os.system(f"node check.js {file} {i}")
    if os.system(f"cargo run {file} {i}") != 0:
        print("system finished")
        exit(0)
    os.system("diff js_output.txt rust_output.txt")
    # if diff fails, exit the program
    if os.system("diff js_output.txt rust_output.txt") != 0:
        print(f"diff failed on step {i}")
        exit(1)
    else:
        print("outputs match")
