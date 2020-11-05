#!/usr/bin/env python3
# Violin plots of full data
import matplotlib.pyplot as plt
import json
import sys
import numpy as np
import os
from tabulate import tabulate
# https://cmdlinetips.com/2019/10/how-to-make-a-plot-with-two-different-y-axis-in-python-with-matplotlib/

FONTSIZE = 12
LINEWIDTH = 1.5
LINEALPHA=1

ANNOTATIONS_1 = {
        "578b8": "LLVM"
}

ANNOTATIONS_3 = {
        "c114a": "IORef → foldM"
        }


BBOX = dict(boxstyle ="round", fc ="0.8") 
ARROWPROPS = { "arrowstyle": "->", 
               "connectionstyle": "angle, angleA = 0, angleB = 90, rad = 10"
             }
if __name__ == "__main__":
    PATH = "./perfdata.gen.json"
    if len(sys.argv) == 2: PATH = sys.argv[1]

    with open(PATH, "r") as f: DATA = json.load(f)


    ORIGTIME = np.average([float(run["total_wall_seconds"]) for run in DATA[0]["rts_data_list"]])
    TABLE = []

    plot_data = []
    # add a dummy final point
    for (ix, datum) in enumerate(DATA):
        # print(datum["commit"])
        rts_runs = datum["rts_data_list"]
        TABLE.append([datum["commit"][:5]] + [r["total_wall_seconds"] for r in rts_runs])

    if os.path.exists("gcc.csv"):
        with open("gcc.csv", "r") as f:
            gcc_data = [float(w) for w in f.readlines()]
        TABLE.append(["gcc"] + gcc_data)

    if os.path.exists("clang.csv"):
        with open("clang.csv", "r") as f:
            clang_data = [float(w) for w in f.readlines()]
        TABLE.append(["clang"] + clang_data)

    print(tabulate(TABLE, tablefmt="latex"))



