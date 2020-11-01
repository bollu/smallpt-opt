#!/usr/bin/env python3
import matplotlib.pyplot as plt
import json
import sys
import numpy as np
import shutil
import os
# https://cmdlinetips.com/2019/10/how-to-make-a-plot-with-two-different-y-axis-in-python-with-matplotlib/

FONTSIZE = 12
LINEWIDTH = 1.5
LINEALPHA=0.7
DUMPDIRPATH="dump-dir"


if __name__ == "__main__":
    PATH = "./perfdata.gen.json"
    if len(sys.argv) == 2: PATH = sys.argv[1]
    with open(PATH, "r") as f: DATA = json.load(f)

    print("clearing out folder[%s]..." % (DUMPDIRPATH, ))
    shutil.rmtree(DUMPDIRPATH, ignore_errors=True)
    print("making folder [%s]..." % (DUMPDIRPATH, ))
    os.mkdir(DUMPDIRPATH)

    for (ix, datum) in enumerate(DATA):
        # print(datum.keys()); sys.exit(0)
        dirname = "%s-%s" % (str(ix + 1), datum["commit"][:5])
        os.mkdir(os.path.join(DUMPDIRPATH, dirname))
        with open(os.path.join(DUMPDIRPATH, dirname, "asm"), "w") as f: f.write(datum["asm"])
        with open(os.path.join(DUMPDIRPATH, dirname, "stg"), "w") as f: f.write(datum["stg"])
        with open(os.path.join(DUMPDIRPATH, dirname, "simpl"), "w") as f: f.write(datum["simpl"])
        print(list(datum.keys()))
        # print(datum["commit"])
        # rts_runs = datum["rts_data_list"]
        # print("===example data==="); print(json.dumps(rts_runs[0], indent=2)); sys.exit(0)
