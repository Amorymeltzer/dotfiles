# Output 42 in ~75s
# From http://codegolf.stackexchange.com/a/20058/8946
from random import randrange as scrabbleBag, randint
from datetime import datetime,timedelta
life,universe,everything,nothing=6,9,1,-3
endOfTheUniverse = 80

tile = lambda i: scrabbleBag(26)
arthur = lambda i: int(`i`,life+universe+everything+nothing)
trillian = lambda i: ''.join(map(str,divmod(i,life+universe+everything+nothing)))

start = datetime.now()

zaphod = lambda: not(randint(0,(endOfTheUniverse-(datetime.now() - start).seconds)**3))
marvin = lambda: endOfTheUniverse<(datetime.now() - start).seconds

answer = None
while answer is not life * universe * everything:
    rack = sum(tile(i) for i in range(7))
    answer = (zaphod() or marvin()) and arthur(rack)
    print trillian(answer)
