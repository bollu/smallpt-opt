#!/usr/bin/env python3
import json

# 9c6c009ace0e46f629d87da19d6a9fd05897c73c
SHA = "9c6c009ace0e46f629d87da19d6a9fd05897c73c"

if __name__ == "__main__":
    PATH = "./perfdata.gen.json"

    with open(PATH, "r") as f: DATA = json.load(f)

    print("reference SHA: |%s|" % (SHA))
    for datum in DATA:
        sha = datum["imagesha"].split()[0]
        print("image-sha: |%20s| | equal?: |5%s|" % (sha, sha == SHA))
