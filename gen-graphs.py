#!/usr/bin/env python3
import matplotlib.pyplot as plt
import json
import sys
import numpy as np

if __name__ == "__main__":
    PATH = "./perfdata.gen.json"
    if len(sys.argv) == 2: PATH = sys.argv[1]

    with open(PATH, "r") as f: DATA = json.load(f)


    plot_data = []
    for (ix, datum) in enumerate(DATA):
        print(datum["commit"])
        rts_runs = datum["rts_data_list"]
        # print(json.dumps(rts_runs[0])); sys.exit(0)

        avg_total_wall_seconds = 0
        for run in rts_runs: avg_total_wall_seconds += float(run["total_wall_seconds"])
        avg_total_wall_seconds = float(avg_total_wall_seconds) /  float(len(rts_runs))

        avg_max_bytes_used = 0
        for run in rts_runs: avg_max_bytes_used += float(run["max_bytes_used"])
        avg_max_bytes_used = float(avg_max_bytes_used) /  float(len(rts_runs))

        plot_data.append({"ix": ix, 
                          "commit": datum["commit"], 
                          "avg_total_wall_seconds": avg_total_wall_seconds,
                          "avg_max_bytes_used": avg_max_bytes_used})
    print(json.dumps(plot_data))


    # https://stackoverflow.com/questions/925024/how-can-i-remove-the-top-and-right-axis-in-matplotlib
    plt.rcParams['axes.spines.right'] = False
    plt.rcParams['axes.spines.top'] = False
    plt.rcParams["font.family"] = "monospace"
    plt.rcParams["font.size"] = 30

    # Time measurements
    BLUE="#2196f3"
    plt.step([datum["ix"] for datum in plot_data], [datum["avg_total_wall_seconds"] for datum in plot_data], color=BLUE, linewidth=3)
    plt.ylabel("wall clock time(s)", color=BLUE)
    plt.yticks(color=BLUE)

    # for datum in plot_data:
    #     plt.stem(datum["ix"], datum["avg_total_wall_seconds"], "bo")
    plt.xticks(np.arange(0, len(plot_data)), [datum["commit"][:5] for datum in plot_data], rotation=90)
    plt.show()

            
