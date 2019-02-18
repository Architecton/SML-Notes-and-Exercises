#!/bin/bash
# Clear worksheets and put blank line between each task instruction.
egrep '^(\()' exercises_worksheet1.sml | sed -e 'G' > temp1.sml
mv temp1.sml exercises_worksheet1.sml
