#!/usr/bin/env python
# tab_cleanup.py by Amory Meltzer
# Encourage me to cleanup my beowser tabs, one a day
### Hall of Shame ###
# 14 Jan 2015 - 181 tabs
# 18 Nov 2015 - 292 tabs

# 18 Nov 2015 - 292 tabs
from datetime import date as D
start_date = D(2015, 1, 14)
start_tabs = int(181)

# Today (18 Nov 2015) - -127 tabs required
import sys
sys.stdout.write('Today (')
print D.today(),
sys.stdout.write('): ')
print start_tabs-(D.today() - start_date).days
