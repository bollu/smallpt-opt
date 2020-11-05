#!/usr/bin/env python3
# Violin plots of full data
import matplotlib.pyplot as plt
import json
import sys
import numpy as np
import os
import tabulate
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

    plot_data = []
    # add a dummy final point
    for (ix, datum) in enumerate(DATA):
        print(datum["commit"])
        rts_runs = datum["rts_data_list"]
        # print("===example data==="); print(json.dumps(rts_runs[0], indent=2)); sys.exit(0)

        plot_data.append({"ix": ix,
                          "commit": datum["commit"], 
                          "total_wall_seconds": [float(r["total_wall_seconds"]) for r in rts_runs]})
                          # "avg_max_bytes_used": avg_max_bytes_used,
                          # "avg_peak_megabytes_allocated": avg_peak_megabytes_allocated})

    # plot_data = [{"ix": 0,
    #                   "commit": "initial",
    #                   "avg_total_wall_seconds": plot_data[0]["avg_total_wall_seconds"],
    #                   "avg_max_bytes_used": plot_data[0]["avg_max_bytes_used"],
    #                   "avg_peak_megabytes_allocated": plot_data[0]["avg_peak_megabytes_allocated"]}] + plot_data

    # add final dummy data point
    plot_data.append({"ix": len(plot_data), 
                      "commit": "final",
                      "total_wall_seconds": plot_data[-1]["total_wall_seconds"]})

    print("===plotting data===")
    print(json.dumps(plot_data, indent=2))
    print("====")

    plt.rcParams["font.family"] = "monospace"
    plt.rcParams["font.size"] = FONTSIZE


    fig,ax = plt.subplots()

    # https://stackoverflow.com/questions/925024/how-can-i-remove-the-top-and-right-axis-in-matplotlib
    ax.spines["right"].set_visible(False)
    ax.spines["top"].set_visible(False)
    # plt.rcParams['axes.spines.right'] = False
    # plt.rcParams['axes.spines.top'] = False

    # Time measurements
    BLUE="#2196f3"
    for datum in plot_data:
        secs = datum["total_wall_seconds"]
        ax.scatter([datum["ix"] for _ in range(len(secs))], secs, 
                    color=BLUE, alpha=LINEALPHA, marker=".", s=9)
    # ax.boxplot(# [datum["ix"] for datum in plot_data],
    #         [datum["total_wall_seconds"] for datum in plot_data]) 
    #         # color=BLUE, linewidth=LINEWIDTH, alpha=LINEALPHA, where='post')
    ax.set_ylabel("wall clock time(s)", color=BLUE)
    ax.tick_params(axis='y', colors=BLUE)
    ax.set_xticks(np.arange(0, len(plot_data)))
    ax.set_xticklabels([datum["commit"][:5] for datum in plot_data], rotation=90)
    ax.set_ylim(ymin=0)

    if os.path.exists("gcc.csv"):
        with open("gcc.csv", "r") as f:
            gcc_data = [float(w) for w in f.readlines()]
        # plot GCC data for each commit
        for datum in plot_data:
            GOLD = "#ff9800"
            ax.scatter([datum["ix"] for _ in range(len(gcc_data))], gcc_data, 
                    color=GOLD, alpha=LINEALPHA, marker="_")

    if os.path.exists("clang.csv"):
        with open("clang.csv", "r") as f:
            clang_data = [float(w) for w in f.readlines()]
        # plot clang data for each commit
        for datum in plot_data:
            BROWN = "#b26a00"
            ax.scatter([datum["ix"] for _ in range(len(clang_data))], clang_data, 
                    color=BROWN, alpha=LINEALPHA, marker="_")

    fig.tight_layout()
    fig.set_size_inches(10, 6)
    fig.savefig("violin.gen.png")
    plt.show()



