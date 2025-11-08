# /// script
# requires-python = ">=3.13"
# dependencies = []
# ///

import csv

def main(ins):
    inp = csv.reader(ins)

    for row in inp:
        print(row)


if __name__ == "__main__":
    import sys
    main(sys.stdin)
