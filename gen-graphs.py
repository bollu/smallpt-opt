#!/usr/bin/env python3
import matplotlib.pyplot as plt
import json
import sys
import numpy as np
import os
import copy
# https://cmdlinetips.com/2019/10/how-to-make-a-plot-with-two-different-y-axis-in-python-with-matplotlib/

FONTSIZE = 12
LINEWIDTH = 2
LINEALPHA=0.7

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

    full_plot_data = []
    # add a dummy final point
    for (ix, datum) in enumerate(DATA):
        print(datum["commit"])
        rts_runs = datum["rts_data_list"]
        # print("===example data==="); print(json.dumps(rts_runs[0], indent=2)); sys.exit(0)

        avg_total_wall_seconds = 0
        for run in rts_runs: avg_total_wall_seconds += float(run["total_wall_seconds"])
        avg_total_wall_seconds = float(avg_total_wall_seconds) /  float(len(rts_runs))
        print("- speedup for |%s|: %4.2f" % 
                    (datum["commit"][:5], ORIGTIME / avg_total_wall_seconds))

        avg_max_bytes_used = 0
        for run in rts_runs: avg_max_bytes_used += float(run["max_bytes_used"])
        avg_max_bytes_used = float(avg_max_bytes_used) /  float(len(rts_runs))

        avg_peak_megabytes_allocated = np.average([float(run["peak_megabytes_allocated"]) for run in rts_runs])


        full_plot_data.append({"ix": ix,
                          "commit": datum["commit"], 
                          "avg_total_wall_seconds": avg_total_wall_seconds,
                          "avg_max_bytes_used": avg_max_bytes_used,
                          "avg_peak_megabytes_allocated": avg_peak_megabytes_allocated})

    # plot_data = [{"ix": 0,
    #                   "commit": "initial",
    #                   "avg_total_wall_seconds": plot_data[0]["avg_total_wall_seconds"],
    #                   "avg_max_bytes_used": plot_data[0]["avg_max_bytes_used"],
    #                   "avg_peak_megabytes_allocated": plot_data[0]["avg_peak_megabytes_allocated"]}] + plot_data

    # add final dummy data point

    for final_ix in range(len(full_plot_data)):

        plot_data = copy.deepcopy(full_plot_data[:final_ix+1])
        plot_data.append({"ix": len(plot_data), 
                          "commit": "final",
                          "avg_total_wall_seconds": plot_data[-1]["avg_total_wall_seconds"],
                          "avg_max_bytes_used": plot_data[-1]["avg_max_bytes_used"],
                          "avg_peak_megabytes_allocated": plot_data[-1]["avg_peak_megabytes_allocated"]})

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
                color=BLUE, linewidth=LINEWIDTH, alpha=LINEALPHA, where='post')
        ax.set_ylabel("wall clock time(s)", color=BLUE)
        ax.tick_params(axis='y', colors=BLUE)
        ax.set_xticks(np.arange(0, len(plot_data)))
        ax.set_xticklabels([datum["commit"][:5] for datum in plot_data], rotation=90)
        ax.set_ylim(ymin=0)

        if os.path.exists("gcc.csv"):
            with open("gcc.csv", "r") as f:
                gcc_data = [float(w) for w in f.readlines()]
                print(gcc_data)
            gcc_avg = np.average(gcc_data)

            GOLD = "#ff9800"
            ax.step([datum["ix"] for datum in plot_data],
                    [gcc_avg for _ in plot_data],
                    color=GOLD, linewidth=LINEWIDTH, alpha=LINEALPHA, where='post')

        if os.path.exists("clang.csv"):
            with open("clang.csv", "r") as f:
                clang_data = [float(w) for w in f.readlines()]
                print(clang_data)
            clang_avg = np.average(clang_data)

            BROWN = "#b26a00"
            ax.step([datum["ix"] for datum in plot_data],
                    [clang_avg for _ in plot_data],
                    color=BROWN, linewidth=LINEWIDTH, alpha=LINEALPHA, where='post')




        OFFSET_1 = (10, 10)
        for datum in plot_data:
            k = datum["commit"][:5]
            if k in ANNOTATIONS_1: 
                ax.annotate(ANNOTATIONS_1[k], 
                                (datum["ix"], datum["avg_total_wall_seconds"]), 
                                xytext=OFFSET_1,
                                textcoords ='offset points')

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

        # =============3

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

        for datum in plot_data:
            k = datum["commit"][:5]
            if k in ANNOTATIONS_3: 
                ax.annotate(ANNOTATIONS_3[k], 
                                (datum["ix"], datum["avg_peak_megabytes_allocated"]))

        # =========LINES==========
        plt.vlines([d["ix"] for d in plot_data], 0, 
                   [max(float(d["avg_peak_megabytes_allocated"]), float(d["avg_total_wall_seconds"])) for d in plot_data],
                   linestyle="dashed", color=(0, 0, 0, 0.3))
        fig.tight_layout()
        fig.set_size_inches(10, 6)
        fig.savefig("perfdata-upto-%s-gen.png" % (plot_data[final_ix]["commit"][:5], ))
        plt.show()



