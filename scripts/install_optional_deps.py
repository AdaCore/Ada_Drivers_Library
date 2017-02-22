#! /usr/bin/env python2

import argparse
import os
import os.path
import subprocess
import sys


ROOT_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))

def run_program(*argv):
    print "$ %s" % " ".join(argv)
    p = subprocess.Popen(
        argv,
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


def git_clone(repo_url, dst, recursive=False):
    extra_args = []

    if recursive:
        extra_args = extra_args + ["--recursive"]

    # Build test drivers
    returncode, stdout, stderr = run_program(
        'git', 'clone', repo_url, ROOT_DIR + dst, *extra_args
    )
    print stdout

    if returncode:
        print 'git clone error (returned {}):\n{}'.format(
            returncode, stderr
        )

    return returncode

# Git repositories
#              Git repository                                       Destination directory            Recursive clone?
git_repos = [("https://github.com/nocko/zfp-nrf51",                 "/examples/MicroBit/zfp-nrf51",  False),
             ("https://github.com/Fabien-Chouteau/zfp-hifive1.git", "/examples/HiFive1/zfp-hifive1", False)
             ]

parser = argparse.ArgumentParser('Download and install optional dependencies')

parser.add_argument(
    'pattern', nargs='*',
    help='List of pattern to filter the set of dependencies to install'
)

def main(args):
    at_least_one_error = False

    ret = 0
    for repo, dest, recursive in git_repos:
        if args.pattern and not any(pat in repo for pat in args.pattern):
            continue

        ret = git_clone (repo, dest, recursive)

        if ret:
            at_least_one_error = True

    if at_least_one_error:
        sys.exit(1)

if __name__ == '__main__':
    main(parser.parse_args())
