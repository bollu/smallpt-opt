#!/usr/bin/env python3
import sys
import git
import sh
import os
import json


# format: [RECORD]
# RECORD: { <ghc-rts-data-name>: value} U { commit, summary, message, diff }

# there is time the keyword in zsh, and time the shell script. Make sure
# we actually run the executable and not the inbuilt nonsense.
TIMEPATH="/usr/bin/time"

# number of runs to collect statistics over
NRUNS = 2


# generate performance data.
# returns the runtimes over NRUNS runs.
def generate_perf_data(repo, c):
    repo.git.checkout(c)
    print("cabal clean...")
    sh.cabal("clean")
    print("cabal build...")
    sh.cabal("build", "--ghc-options", "-ddump-to-file",  "--ghc-options", "-ddump-simpl", "--ghc-options", "-ddump-stg", "--ghc-options", "-ddump-asm")
    print("looking up executable...")
    cwd = os.getcwd()
    # NOTE: we need the .strip() to eliminate trailing newlines
    exepath = str(sh.find("dist-newstyle", "-name", "smallpt-opt", "-type", "f")).strip()
    print("found executable: |%s|" % (exepath, ))
    exepathabs = os.path.join(cwd, exepath)
    print("timing executable at path: |%s|" % (exepathabs, ))
    exe = sh.Command(exepathabs)

    ts = []
    rts_data_list = []
    for i in range(NRUNS):
        print("run (%s/%s)" % (i+1, NRUNS))
        # https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime_control.html#rts-options-to-produce-runtime-statistics
        rts_data = dict(eval(str(exe("+RTS", "-t", "--machine-readable",  _err_to_out=True, _out=None)).strip()))
        print(json.dumps(rts_data, indent=2))
        t = rts_data["mut_wall_seconds"]
        ts.append(t)
        rts_data_list.append(rts_data)
    print("times: %s" % (ts, ))

    simplpath = str(sh.find("dist-newstyle", "-name", "smallpt.dump-simpl", "-type", "f")).strip()
    with open(simplpath, "r") as f: simpl = f.read()

    asmpath = str(sh.find("dist-newstyle", "-name", "smallpt.dump-asm", "-type", "f")).strip()
    with open(asmpath, "r") as f: asm = f.read()

    stgpath = str(sh.find("dist-newstyle", "-name", "smallpt.dump-stg", "-type", "f")).strip()
    with open(stgpath, "r") as f: stg = f.read()

    return {"times": ts, "rts_data_list" : rts_data_list, "simpl": simpl, "asm": asm, "stg": stg }


# We hardcode paths, sorry
# TODO: for whatever reason, in the case of the first patch,
# I can't generate a diff.
if __name__ == "__main__":
    # array of dicts
    out_commits_data = []

    repo = git.Repo(".")
    cs_old2new = list(repo.iter_commits("master"))
    cs_old2new.reverse()

    for c in cs_old2new:
        print("at commit: |%s|" % c)
        diff = c.diff(create_patch=True)
        diff_str = "" if not diff else diff[0].diff.decode("utf-8")
        out = generate_perf_data(repo, c)
        out.update({ "commit": c.hexsha,
                     "summary": c.summary, 
                     "message": c.message, 
                     "diff": diff_str
                    })
        out_commits_data.append(out)

        with open("perfdata.gen.json", "w") as of: json.dump(out_commits_data, of)
        # print(cs_old2new[i+1].message)
        # print(cs_old2new[i+1].summary)
        # print (cs_old2new[i+1], cs_old2new[i])
        # I have no idea why this returns a list. I only need and have only
        # seen a single diff. Maybe something to do with meerges?
        # diff = cs_old2new[i].diff(cs_old2new[i+1], create_patch=True)[0]
        # # print("vvvvvvvvvv")
        # print(diff.diff.decode("utf-8"))
        # generate_perf_data(repo, cs_old2new[i+1])
        # sys.exit(0)
        # print("^^^^^^^^^")
    # at the end, check out scripting
    # TODO: change this to check out master when done. Or better yet, bring
    # back to last known state.
    # repo.git.checkout('scripting')
    # print(out_commits_data)