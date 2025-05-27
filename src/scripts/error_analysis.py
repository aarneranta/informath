import sys
import json

INFILE = sys.argv[1]

with open(INFILE) as file:
    data = [json.loads(line) for line in file]

for d in data:
    for k, v in d.items():
        print(k +':', v)

