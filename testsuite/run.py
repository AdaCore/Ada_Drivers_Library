#! /usr/bin/env python3

import argparse
import difflib
import os
import os.path
import subprocess


ROOT_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
TESTS_DIR = os.path.join(ROOT_DIR, 'testsuite', 'tests')


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
        return 'stdout is not ASCII'

    try:
        stderr = stderr.decode('ascii')
    except UnicodeError:
        return 'stderr is not ASCII'

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

    def run(self):
        """
        Build and run the test drivers for this testcase, then check their
        output. Return a string as an error message if there is an error.
        Return None if test is successful.
        """

        # Build test drivers
        returncode, stdout, stderr = run_program(
            'gprbuild', '-j0', '-p', '-q', '-P', self.project_file,
        )
        if returncode:
            return 'Build error (gprbuild returned {}):\n{}'.format(
                returncode, stderr
            )

        # Run individual testcases
        errors = []
        for program, expected_output_fn in self.drivers:
            error = self._run_single(program, expected_output_fn)
            if error:
                errors.append('{}:\n{}\n'.format(
                    os.path.basename(program),
                    error
                ))

        return '\n'.join(errors) if errors else None

    def _run_single(self, program, expected_output_fn):
        """
        Helper for run, execute a single test driver.
        """
        # Run the program, get its output
        returncode, stdout, stderr = run_program(program)
        if returncode or stderr:
            return 'Program returned {}:\n{}'.format(returncode, stderr)

        stdout = stdout.splitlines()

        # Compare the actual output and the expected one
        with open(expected_output_fn, 'r') as f:
            expected_output = f.read().splitlines()
        if expected_output != stdout:
            return 'Output mismatch:\n{}'.format(''.join(
                difflib.unified_diff(
                    expected_output,
                    stdout,
                    fromfile=expected_output_fn,
                    tofile='<stdout>'
                )
            ))



def find_testcases():
    """
    Yield Testcase instances for all testcases found in TESTS_DIR.
    """
    for dirpath, dirnames, filenames in os.walk(TESTS_DIR):
        if 'tc.gpr' in filenames:
            yield Testcase(dirpath)


parser = argparse.ArgumentParser('Run the testsuite')

parser.add_argument(
    'pattern', nargs='*',
    help='List of pattern to filter the set of testcases to run'
)


def main(args):
    for tc in find_testcases():

        # Don't run the testcase if we have filters and none of them matches it
        if args.pattern and not any(pat in tc.name for pat in args.pattern):
            continue

        error = tc.run()
        if error:
            print('\x1b[31mFAIL\x1b[0m {}:\n{}'.format(tc.name, error))
        else:
            print('\x1b[32mOK\x1b[0m   {}'.format(tc.name))


if __name__ == '__main__':
    main(parser.parse_args())
