#!/usr/bin/env python
# coding=UTF-8
# Display battery percentage
# http://stevelosh.com/blog/2010/02/my-extravagant-zsh-prompt/#my-right-prompt-battery-capacity
# Needs tweaking? ;;;;;; ##### FIXME TODO

# Some code to use, perhaps
# battt=$( ioreg -l | grep -i capacity | tr '\n' ' | ' | awk '{printf("%.2f%%", $10/$5 * 100)}')
# if [[ -z $battt ]]; then
# echo "got da powah"
# else
# echo $battt
# fi

import math, subprocess

p = subprocess.Popen(["ioreg", "-rc", "AppleSmartBattery"], stdout=subprocess.PIPE)
output = p.communicate()[0]

o_max = [l for l in output.splitlines() if 'MaxCapacity' in l][0]
o_cur = [l for l in output.splitlines() if 'CurrentCapacity' in l][0]

b_max = float(o_max.rpartition('=')[-1].strip())
b_cur = float(o_cur.rpartition('=')[-1].strip())

charge = b_cur / b_max
charge_threshold = int(math.ceil(10 * charge))


#charging = ["☿" for l in output.splitlines() if 'IsCharging\" = No' in l][0]
#charging = ["⚡" for l in output.splitlines() if 'IsCharging\" = Yes' in l][0]
#charging = ["⚡" for l in output.splitlines() if 'FullyCharged\" = Yes' in l][0]
charging = [l for l in output.splitlines() if 'IsCharging\" = ' in l][0]

tester = charging.rpartition('=')[-1].strip()
if tester == "Yes":
    charging = "⚡"
else:
    charging = "☿"


# Output
# ⚡ → ↑ ↓ ↕ ○ ☿ ± ✘ ¤ « ¬ ¼ ½ ¾ × ƴ ˃ ˧ ૦ ᐅ ᗆ ᗌ ᗒ ᗘ ↀ ⇛ ⇒ ⇨ ↝ ∇ ⋕ ⌁ ⌇ ⎋ ⏆
# ▶ ▷ ▸ ▹ ► ▻ ◆ ◇ ◈ ◊ ☇ ☈ ✈ ➤ ➙ ⨠ 𝆓

total_slots, slots = 10, []
filled = int(math.ceil(charge_threshold * (total_slots / 10.0))) * u'▸'
empty = (total_slots - len(filled)) * u'▹'


out = (filled + empty).encode('utf-8')
import sys

color_green = '\033[0;32m'
color_yellow_intense = '\033[0;93m'
color_red = '\033[0;31m'
color_red_blink = '\033[5;31m'
color_reset = '\033[0m'
color_out = (
    color_green if len(filled) > 7
    else color_yellow_intense if len(filled) > 4
    else color_red if len(filled) > 2
    else color_red_blink
)

out = color_out + charging + out + color_reset
sys.stdout.write(out)
