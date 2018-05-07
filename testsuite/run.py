#! /usr/bin/env python2

import argparse
import difflib
import os
import os.path
import subprocess
import sys


ROOT_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
TESTS_DIR = os.path.join(ROOT_DIR, 'testsuite', 'tests')
COV_DIR = os.path.join(ROOT_DIR, 'testsuite', 'coverage_results')


def run_program(*argv):
    p = subprocess.Popen(
        argv,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE
    )
    stdout, stderr = p.communicate()

    try:
        stdout = stdout.decode('ascii')
    except UnicodeError:
        return (p.returncode, 'stdout is not ASCII', stderr)

    try:
        stderr = stderr.decode('ascii')
    except UnicodeError:
        return (p.returncode, stdout, 'stderr is not ASCII')

    return (p.returncode, stdout, stderr)


class Testcase:
    def __init__(self, dirname):
        self.name = dirname
        self.dirname = os.path.join(TESTS_DIR, dirname)
        self.project_file = os.path.join(dirname, 'tc.gpr')

    @property
    def expected_outputs(self):
        """
        Yield the basename for all expected output files in this testcase.
        """
        for fn in os.listdir(self.dirname):
            if fn.endswith('.out'):
                yield fn

    @property
    def drivers(self):
        """
        Return a couple (program full path, expected output full path) for all
        test drivers in this testcase.
        """
        return [(os.path.join(self.dirname, 'bin', fn[:-4]),
                 os.path.join(self.dirname, fn))
                for fn in self.expected_outputs]

    def run(self, args):
        """
        Build and run the test drivers for this testcase, then check their
        output. Return a string as an error message if there is an error.
        Return None if test is successful.
        """

        build_args = ['gprbuild', '-f', '-j0', '-p', '-q', '-P', self.project_file]

        if args.coverage:
            build_args += ['-largs', '-lgcov', '-cargs', '-fprofile-arcs', '-ftest-coverage']

        # Build test drivers
        returncode, stdout, stderr = run_program(*build_args)

        if returncode:
            return 'Build error (gprbuild returned {}):\n{}'.format(
                returncode, stderr
            )

        if len(self.drivers) == 0:
            return "No testcase to execute...\n"

        # Run individual testcases
        errors = []
        for program, expected_output_fn in self.drivers:
            error = self._run_single(args, program, expected_output_fn)
            if error:
                errors.append('{}:\n{}\n'.format(
                    os.path.basename(program),
                    error
                ))

        return '\n'.join(errors) if errors else None

    def _run_single(self, args, program, expected_output_fn):
        """
        Helper for run, execute a single test driver.
        """
        # Get the expected output
        with open(expected_output_fn, 'r') as f:
            expected_output = f.read()
            try:
                expected_output = expected_output.decode('ascii')
            except UnicodeError:
                return 'Expected output is not ASCII'
            expected_output = expected_output.splitlines()

        # Run the program, get its output
        argv = [program]
        if args.valgrind:
            argv = ['valgrind', '-q', '--leak-check=full'] + argv

        returncode, stdout, stderr = run_program(*argv)

        program_returned_msg = \
            'Program returned {}:\n{}'.format(returncode, stderr)

        if returncode or stderr:
            return program_returned_msg
        elif args.verbose:
            print program_returned_msg

        stdout = stdout.splitlines()

        # Compare the actual output and the expected one
        if expected_output != stdout:
            return 'Output mismatch:\n{}'.format('\n'.join(
                difflib.unified_diff(
                    expected_output,
                    stdout,
                    fromfile=expected_output_fn,
                    tofile='<stdout>'
                )
            ))
        elif args.verbose:
            print "\n".join(stdout)


def find_testcases():
    """
    Yield Testcase instances for all testcases found in TESTS_DIR.
    """
    for dirpath, dirnames, filenames in os.walk(TESTS_DIR):
        if 'tc.gpr' in filenames:
            yield Testcase(dirpath)


def gcda_files():
    l = []
    for root, dirs, files in os.walk(ROOT_DIR):
        for file in files:
            if file.endswith(".gcda") and not "b__" in file:
                l.append(os.path.join(root, file))
    return l


def coverage_analysis():

    files = gcda_files()

    if not os.path.exists(COV_DIR):
        os.makedirs(COV_DIR)

    print "Coverage analysis (%d files)..." % len(files)
    os.chdir(COV_DIR)
    run_program(*(['gcov'] + files))


parser = argparse.ArgumentParser('Run the testsuite')

parser.add_argument(
    '--valgrind', action='store_true',
    help='Use Valgrind to detect invalid memory operations and leaks'
)

parser.add_argument(
    '--verbose', action='store_true',
    help='Print exit code and output for all tests, regardless of results'
)

parser.add_argument(
    '--coverage', action="store_true",
    help="Compute testsuite code coverage",
    default=False
)

parser.add_argument(
    'pattern', nargs='*',
    help='List of pattern to filter the set of testcases to run'
)


def main(args):
    at_least_one_error = False
    for tc in find_testcases():

        # Don't run the testcase if we have filters and none of them matches it
        if args.pattern and not any(pat in tc.name for pat in args.pattern):
            continue

        error = tc.run(args)
        if error:
            at_least_one_error = True
            print('\x1b[31mFAIL\x1b[0m {}:\n{}'.format(tc.name, error))
        else:
            print('\x1b[32mOK\x1b[0m   {}'.format(tc.name))

    if args.coverage:
        coverage_analysis()

    if at_least_one_error:
        sys.exit(1)

if __name__ == '__main__':
    main(parser.parse_args())
