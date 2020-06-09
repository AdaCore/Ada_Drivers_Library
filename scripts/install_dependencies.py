#! /usr/bin/env python3

import argparse
import os
import os.path
import subprocess
import sys
import shutil

ROOT_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), '../'))

def run_program(argv, cwd=os.path.dirname(os.path.realpath(__file__))):
    print("$ %s" % " ".join(argv))
    p = subprocess.Popen(
        argv,
        cwd=cwd,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE
    )
    stdout, stderr = p.communicate()

    try:
        stdout = stdout.decode('ascii')
    except UnicodeError:
        return 'stdout is not ASCII'

    try:
        stderr = stderr.decode('ascii')
    except UnicodeError:
        return 'stderr is not ASCII'

    return (p.returncode, stdout, stderr)


def git_clone(repo_url, branch, dst, recursive=False):
    extra_args = []

    if recursive:
        extra_args = extra_args + ["--recursive"]

    # Clone the repo
    returncode, stdout, stderr = run_program(
        ['git', 'clone', repo_url, dst] + extra_args)
    print(stdout)

    if returncode:
        print('git clone error (returned {}):\n{}'.format(
            returncode, stderr
        ))
        return returncode

    if branch:
        # Clone the repo
        returncode, stdout, stderr = run_program([
            'git', '-C', dst, 'checkout', '-b', branch, "origin/" + branch])
        print(stdout)

        if returncode:
            print('git branch checkout error (returned {}):\n{}'.format(
                returncode, stderr
                ))
    return returncode

# Git repositories
#  - Git repository URL
#  - Git branch (if not master)
#  - Destination directory
#  - Recursive clone?
#  - install command (if any)
git_repos = [("https://github.com/AdaCore/bb-runtimes",
              False,
              "bb-runtimes",
              False,
              [sys.executable, ROOT_DIR + "/bb-runtimes/install.py",
               "--arch=arm-eabi",
               "--prefix=" +
               os.path.join(os.path.dirname(shutil.which("arm-eabi-gnatls")),
                            '..', 'arm-eabi', 'lib', 'gnat')]),
             ]

parser = argparse.ArgumentParser('Download and install dependencies')

parser.add_argument(
    'pattern', nargs='*',
    help='List of pattern to filter the set of dependencies to install'
)


def main(args):
    at_least_one_error = False

    print("ROOT_DIR :" + ROOT_DIR)
    ret = 0
    for repo, branch, dest, recursive, build_cmd in git_repos:
        if args.pattern and not any(pat in repo for pat in args.pattern):
            continue

        dest = os.path.join(ROOT_DIR, dest)

        if not os.path.exists(dest):
            ret = git_clone(repo, branch, dest, recursive)
        else:
            print("%s already cloned" % dest)

        if ret:
            at_least_one_error = True

        if build_cmd:
            print("Running build command:")

            ret, stdout, stderr = run_program(build_cmd, dest)

            print(stdout)

            if ret:
                print('Dependency install command error' +\
                      ' (returned {}):\n{}'.format(ret, stderr))
                at_least_one_error = True

    if at_least_one_error:
        sys.exit(1)

if __name__ == '__main__':
    main(parser.parse_args())
