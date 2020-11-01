#!/usr/bin/env python3
import matplotlib.pyplot as plt
import json
import sys
import numpy as np
# https://cmdlinetips.com/2019/10/how-to-make-a-plot-with-two-different-y-axis-in-python-with-matplotlib/

FONTSIZE = 12
LINEWIDTH = 1.5
LINEALPHA=0.7

if __name__ == "__main__":
    PATH = "./perfdata.gen.json"
    if len(sys.argv) == 2: PATH = sys.argv[1]

    with open(PATH, "r") as f: DATA = json.load(f)


    plot_data = []
    for (ix, datum) in enumerate(DATA):
        print(datum["commit"])
        rts_runs = datum["rts_data_list"]
        # print("===example data==="); print(json.dumps(rts_runs[0], indent=2)); sys.exit(0)

        avg_total_wall_seconds = 0
        for run in rts_runs: avg_total_wall_seconds += float(run["total_wall_seconds"])
        avg_total_wall_seconds = float(avg_total_wall_seconds) /  float(len(rts_runs))

        avg_max_bytes_used = 0
        for run in rts_runs: avg_max_bytes_used += float(run["max_bytes_used"])
        avg_max_bytes_used = float(avg_max_bytes_used) /  float(len(rts_runs))

        avg_peak_megabytes_allocated = np.average([float(run["peak_megabytes_allocated"]) for run in rts_runs])


        plot_data.append({"ix": ix, 
                          "commit": datum["commit"], 
                          "avg_total_wall_seconds": avg_total_wall_seconds,
                          "avg_max_bytes_used": avg_max_bytes_used,
                          "avg_peak_megabytes_allocated": avg_peak_megabytes_allocated})

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
    ax.step([datum["ix"] for datum in plot_data],
            [datum["avg_total_wall_seconds"] for datum in plot_data], 
            color=BLUE, linewidth=LINEWIDTH, alpha=LINEALPHA)
    ax.set_ylabel("wall clock time(s)", color=BLUE)
    ax.tick_params(axis='y', colors=BLUE)
    ax.set_xticks(np.arange(0, len(plot_data)))
    ax.set_xticklabels([datum["commit"][:5] for datum in plot_data], rotation=90)

    # https://cmdlinetips.com/2019/10/how-to-make-a-plot-with-two-different-y-axis-in-python-with-matplotlib/
    #
    PINK="#e91e63"
    ax2=ax.twinx()
    ax2.spines["top"].set_visible(False)
    ax2.step([datum["ix"] for datum in plot_data], 
             [datum["avg_max_bytes_used"] for datum in plot_data], 
             color=PINK, linewidth=LINEWIDTH, alpha=LINEALPHA)
    ax2.set_ylabel("maximum bytes used (bytes)", color=PINK)
    ax2.tick_params(axis='y', colors=PINK)
    # for datum in plot_data:
    #     plt.stem(datum["ix"], datum["avg_total_wall_seconds"], "bo")


    GREEN="#8bc34a"
    ax3=ax.twinx()
    ax3.spines["top"].set_visible(False)
    ax3.step([datum["ix"] for datum in plot_data],
             [datum["avg_peak_megabytes_allocated"] for datum in plot_data], 
             color=GREEN, linewidth=LINEWIDTH, alpha=LINEALPHA)
    ax3.set_ylabel("peak megabytes allocated (MB)", color=GREEN)
    ax3.tick_params(axis='y', colors=GREEN)
    # https://matplotlib.org/3.1.1/gallery/ticks_and_spines/multiple_yaxis_with_spines.html
    ax3.spines["right"].set_position(("axes", 1.1))

    fig.tight_layout()
    fig.set_size_inches(10, 6)
    fig.savefig("perfdata.gen.png")
    plt.show()

