#! /usr/bin/env python3

import os
import sys
import config
import config.user_input.console
from build_all_examples import run_program

from config.boards import list_of_boards

script_dir = os.path.dirname(__file__)
ADL_root_dir = os.path.abspath(os.path.join(script_dir, ".."))
top_readme_file = os.path.join(ADL_root_dir, "README.md")

undocumented = []

error = \
"""Next boards were not mentioned in the top README.md file!
Please, append each of them to the table in the Board List section.
"""

with open(top_readme_file, 'r', encoding='utf-8') as file:
    lines = [line for line in file]

# No error on Custom_Board:
lines.append("[Custom_Board]")

for board in list_of_boards():
    text = f"[{board}]"
    found = [line for line in lines if text in line]
    if not found:
        undocumented.append(board)

if undocumented:
    print(error)
    print(undocumented)
    sys.exit(1)
