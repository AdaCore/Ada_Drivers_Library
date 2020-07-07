#! python

import sys


def valid_int(str):
    try:
        int(str)
        return True
    except ValueError:
        return False


def valid_float(str):
    try:
        float(str)
        return True
    except ValueError:
        return False


def query_bool(question, default="yes"):
    valid = {"yes": 'True', "y": 'True', "ye": 'True', "True": 'True',
             "no": 'False', "n": 'False', "False": 'False'}
    if default is None and default not in valid:
        prompt = " [y/n]\n"
    elif default == "yes" or default == 'y' or default == "True":
        prompt = " [Y/n]\n"
    elif default == "no" or default == 'n' or default == "False":
        prompt = " [y/N]\n"
    else:
        raise ValueError("invalid default answer: '%s'" % default)

    while True:
        sys.stdout.write(question + prompt)
        choice = input().lower()
        if choice == '?':
            continue
        elif default is not None and choice == '':
            return valid[default]
        elif choice in valid:
            return valid[choice]
        else:
            sys.stdout.write("Please respond with 'yes' or 'no' "
                             "(or 'y' or 'n').\n")


def query_string(question, default):

    prompt = " [default: '%s']\n? " % (default)
    while True:
        sys.stdout.write(question + prompt)
        choice = input()
        if choice == '?':
            continue
        elif default is not None and choice == '':
            return default
        else:
            return choice


def query_choice(question, choices, default):

    while True:
        print(question)
        cnt = 0
        for item in choices:
            print(" - (%d) %s" % (cnt, item))
            cnt += 1
        sys.stdout.write("? ")
        choice = input()
        if choice == '?':
            continue
        elif default is not None and choice == '':
            return default
        elif choice in choices:
            return choice
        elif valid_int(choice) and 0 <= int(choice) <= len(choices) - 1:
            return choices[int(choice)]
        else:
            sys.stdout.write("Please respond with an item of the list.\n")


def query_int(question, range_from, range_to, default):

    has_range = range_from is not None and range_to is not None

    if has_range and range_from > range_to:
        raise ValueError("invalid range : %d .. %d" % (range_from, range_to))

    if has_range and default is not None and \
       not range_from <= default <= range_to:
        raise ValueError("invalid default answer: %d" % default)

    if has_range:
        prompt = " [%d .. %d] default:%s\n" % (range_from, range_to, default)
    else:
        prompt = " [default: %s]\n" % (default)

    while True:
        sys.stdout.write(question + prompt)
        choice = input().lower()
        if choice == '?':
            continue
        elif default is not None and choice == '':
            return default
        elif not valid_int(choice):
            sys.stdout.write("'%s' is not a valid int value\n" % choice)
        elif not has_range or range_from <= int(choice) <= range_to:
            return int(choice)
        else:
            sys.stdout.write("'%s' is not in the range of valid values\n" %
                             choice)


def query_float(question, range_from, range_to, default):

    has_range = range_from is not None and range_to is not None

    if has_range and range_from > range_to:
        raise ValueError("invalid range : %d .. %d" % (range_from, range_to))

    if has_range and default is not None and \
       not range_from <= default <= range_to:
        raise ValueError("invalid default answer: %d" % default)

    if has_range:
        prompt = " [%d .. %d] default:%s\n" % (range_from, range_to, default)
    else:
        prompt = " [default: %s]\n" % (default)

    while True:
        sys.stdout.write(question + prompt)
        choice = input().lower()
        if choice == '?':
            continue
        elif default is not None and choice == '':
            return default
        elif not valid_int(choice):
            sys.stdout.write("'%s' is not a valid float value\n" % choice)
        elif not has_range or range_from <= float(choice) <= range_to:
            return int(choice)
        else:
            sys.stdout.write("'%s' is not in the range of valid values\n" %
                             choice)
