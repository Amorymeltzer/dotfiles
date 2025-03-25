;;; amory-reference-functions.el --- Reference functions -*- lexical-binding: t; -*-
;;; Mainly from <http://svn.red-bean.com/repos/kfogel/trunk/.emacs>

;; The genetic code stuff needs to fixed FIXME TODO
;; Add some of John McCarthy units: TODO
;; <http://www-formal.stanford.edu/jmc/facts.txt> and <https://github.com/armcknight/john-mccarthy-numerical-facts/tree/main>

;; Reference tables
(defconst amory-recentf-help
  "         Bindings for opening
C-x C-f               Open file
C-x f                 Recentf
C-c C-r, C-c r        Recentf with IDO
C-x C-r               Reopen last closed
C-c C-f               Open file in other window
C-c f                 Open file in other window, don't select
C-c C-b               Open buffer in other window
C-c b, C-M-f          Open buffer in other window, don't select"
  "Cheat sheet for recentf opening.")


(defconst amory-latin-abbreviation-help
  "         http://en.wikipedia.org/wiki/List_of_Latin_abbreviations

* A.D.  |  anno Domini  |  \"in the year of the Lord\"

  Used to label or number years in the Julian and Gregorian
  calendars. The AD or the Christian calendar era is based on the
  traditionally reckoned year of the conception or birth of Jesus of
  Nazareth, with AD counting years after the start of this epoch, and
  BC denoting years before the start of the epoch.  Example: The
  United States Civil War began in AD 1861

* a.m.  |  Ante Meridiem  |  \"before midday\"

  Used on the twelve-hour clock to indicate times during the morning.
  Example: We will meet the mayor at 10 a.m. (10:00 in 24hour-clock)

* c., ca., ca or cca.  |  circa  |  \"around\", \"about\", \"approximately\"

  Used in dates to indicate approximately.  Example: The antique clock
  is from c.1900.

* Cap.  |  capitulus  |  \"chapter\"

  Used before a chapter number of laws of the United Kingdom and
  its (former) colonies.  Example: Electronic Transactions Ordinance
  (Cap. 553).'

* cf.  |  confer  |  \"bring together\" and hence \"compare\"

  Confer is the imperative of the Latin verb conferre.  Used
  interchangeably with \"cp.\" in citations indicating the reader should
  compare a statement with that from the cited source.  Example: These
  results were similar to those obtained using different techniques
  (cf. Wilson, 1999 and Ansmann, 1992).

* cp.  |   | compare

  Used interchangeably with \"cf.\" in citations indicating the reader
  should compare a statement with that from the cited source.
  Example: These results were similar to those obtained using
  different techniques (cp. Wilson, 1999 and Ansmann, 1992).

* Cp  |  ceteris paribus  |  \"all other things equal\"

* C.V. or CV | curriculum vitae | \"course of life\"

  A document containing a summary or listing of relevant job
  experience and education. The exact usage of the term varies between
  British English and American English.

* cwt.  |  centum weight  |  \"Hundredweight\"

  cwt. uses a mixture of Latin and English abbreviation.

* D.V.  |  Deo volente  |  \"God willing\"

* DG, D.G. or DEI GRA | Dei gratia | \"by the grace of God\".

  A part of the monarch's title, it is found on all British and
  Canadian coins.

* ead.  |  eadem  |  see id. below.

* et al.  |  et alii | \"and others\", \"and co-workers\".

  It can also stand for et alia, \"and other things\", or et alibi, \"and
  other places\".  Example: These results agree with the ones published
  by Pelon et al. (2002).

* etc.  |  et cetera  |  \"and the others\", \"and other things\", \"and the rest\".

  Other archaic abbreviations include \"&c.\", \"&/c.\", \"&e.\", \"&ct.\",
  and \"&ca.\"  Example: I need to go to the store and buy some pie,
  milk, cheese, etc.

* e.g.  |  exempli gratia  |  \"for example\", \"for instance\".

  Example: The shipping company instituted a surcharge on any items
  weighing over a ton; e.g., a car or truck.

* ff.  |  folio  |  \"and following\"

  This abbreviation is used in citations to indicate an unspecified
  number of following pages following, Example: see page 258ff.

* ibid.  |  ibidem  |  \"in the same place (book, etc.)\"

  The abbreviation is used in citations. It should not be confused
  with the following abbreviation. It is better pronounced ibídem,
  with stress on the second -i- (as it was in Latin).

* id.  |  idem  |  \"the same (man)\".

  It is used to avoid repeating the name of a male author (in
  citations, footnotes, bibliographies, etc.) When quoting a female
  author, use the corresponding feminine form, ead. (eadem), \"the same
  (woman)\" (eadem is pronounced with stress on the first e-).

* i.a.  |  inter alia  |  \"among other things\".

  Example: Ernest Hemingway—author (i.a. 'The Sun Also Rises') and
  friend.

* i.e.  |  id est  |  \"that is\", \"in other words\".

* J.D.  |  Juris Doctor  |  \"teacher of law/rights\".

* lb.  |  libra | \"scales\"

  Used to indicate the pound (mass).

* LL.B.  |  Legum Baccalaureus  |  \"bachelor of laws\"

  The \"LL.\" of the abbreviation for the degree is from the genitive
  plural legum (of lex, legis f., law), thus \"LL.B.\" stands for Legum
  Baccalaureus in Latin. In the United States it was sometimes
  erroneously called \"Bachelor of Legal Letters\" to account for the
  double \"L\" (and therefore sometimes abbreviated as \"L.L.B.\").

* M.A.  |  Magister Artium  |  \"Master of Arts\"

  A postgraduate academic master degree awarded by universities in
  many countries. The degree is typically studied for in fine art,
  humanities, social science or theology and can be either fully
  taught, research-based, or a combination of the two.

* M.O.  |  modus operandi  |  \"method of operating\"

  Sometimes used in criminology to refer to a criminal's method of
  operation.

* N.B.  |  nota bene  |  \"note well\"

  Some people use \"Note\" for the same purpose.  Usually written with
  majuscule (French upper case / 'capital') letters.  Example: N.B.:
  All the measurements have an accuracy of within 5% as they were
  calibrated according to the procedure described by Jackson (1989).

* nem. con.  |  nemine contradicente  |  \"with no one speaking against\"

  The meaning is distinct from \"unanimously\"; \"nem. con.\" simply means
  that nobody voted against. Thus there may have been abstentions from
  the vote.

* op. cit.  |  opere citato  |  \"the work cited\"

  Means in the same article, book or other reference work as was
  mentioned before. It is most often used in citations in a similar
  way to \"ibid\", though \"ibid\" would usually be followed by a page
  number.

* p.a.  |  per annum  |  \"through a year\"

  Is used in the sense of \"yearly\".

* per cent.  |  per centum  |  \"for each one hundred\"

  Commonly \"percent\"

* Ph.D.  |  Philosophiæ Doctor  |  \"Teacher of Philosophy\"

* P.M.  |  Post Meridiem | \"after midday\"

  Used on the twelve-hour clock to indicate times during the
  afternoon.  Example: We will meet the mayor at 2 P.M. (14:00 in
  24hour-clock)

* p.m.a.  |  post mortem auctoris  |  \"after the author's death\"

* p.p. and per pro.  |  per procurationem | \"through the agency of\"

* PRN | pro re nata | \"as needed\"

  Used in prescriptions

* pro tem.  |  pro tempore  |  \"for the time being\", \"temporarily\", \"in place of\"

* P.S.  |  post scriptum  |  \"after what has been written\"

  it is used to indicate additions to a text after the signature of a
  letter.

* Q.D.  |  quaque die  |  \"every day\"

  Used on prescriptions to indicate the medicine should be taken
  daily.

* Q.E.D.  |  quod erat demonstrandum  |  \"which was to be demonstrated\".

  Cited in many texts at the end of a mathematical proof.  Example: At
  the end of the long proof, the professor exclaimed \"Alas, Q.E.D!\"

* q.v.  |  quod videre  |  \"which to see\"

  Used as an imperative.  Used after a term or phrase that should be
  looked up elsewhere in the current document or book. For more than
  one term or phrase, the plural is quae videre (qq.v.).

* Re  |  in re  |  \"in the matter of\", \"concerning\"

  Often used to prefix the subject of traditional letters and
  memoranda. However, when used in an e-mail subject, there is
  evidence that it functions as an abbreviation of \"reply\" rather than
  the word meaning \"in the matter of\". Nominative case singular 'res'
  is the Latin equivalent of 'thing'; singular 're' is the ablative
  case required by 'in'. Some people believe it is short for
  'regarding'.

* REG  |  regina  |  \"queen\"

  A part of the monarch's title, it is found on all British coins
  minted during the reign of a monarch who is a queen. Rex, \"king\"
  (not an abbreviation) is used when the reigning monarch is a king.

* R.I.P.  |  requiescat in pace  |  \"may he/she rest in peace\"

  Used as a short prayer for a dead person, frequently found on
  tombstones. \"R.I.P.\" can also mean requiescant in pace, which is the
  plural form and translates to \"may they rest in peace\" Example:
  R.I.P good grandmother.

* s.o.s.  |  si opus sit  |  \"if there is need\", \"if occasion require\", \"if necessary\"

* stat.  |  statim | \"immediately\"

  Often used in medical contexts.  Example: That patient needs
  attention, stat.!

* viz.  |  videlicet  |  \"namely\", \"to wit\", \"precisely\", \"that is to say\"

  In contradistinction to \"i.e.\" and \"e.g.\", \"viz.\" is used to
  indicate a detailed description of something stated before, and when
  it precedes a list of group members, it implies (near) completeness.
  Example: The noble gases, viz. helium, neon, argon, xenon, krypton
  and radon, show a non-expected behaviour when exposed to this new
  element.

* vs or v.  |  versus  |  \"against\"

  Sometimes is not abbreviated.  Example: The next football game will
  be the Knights vs. the Sea Eagles.
")


(defconst amory-ascii
  "
       Decimal - Character

       |  0 NUL|  1 SOH|  2 STX|  3 ETX|  4 EOT|  5 ENQ|  6 ACK|  7 BEL|
       |  8 BS |  9 HT | 10 NL | 11 VT | 12 NP | 13 CR | 14 SO | 15 SI |
       | 16 DLE| 17 DC1| 18 DC2| 19 DC3| 20 DC4| 21 NAK| 22 SYN| 23 ETB|
       | 24 CAN| 25 EM | 26 SUB| 27 ESC| 28 FS | 29 GS | 30 RS | 31 US |
       | 32 SP | 33  ! | 34  \" | 35  # | 36  $ | 37  % | 38  & | 39  ' |
       | 40  ( | 41  ) | 42  * | 43  + | 44  , | 45  - | 46  . | 47  / |
       | 48  0 | 49  1 | 50  2 | 51  3 | 52  4 | 53  5 | 54  6 | 55  7 |
       | 56  8 | 57  9 | 58  : | 59  ; | 60  < | 61  = | 62  > | 63  ? |
       | 64  @ | 65  A | 66  B | 67  C | 68  D | 69  E | 70  F | 71  G |
       | 72  H | 73  I | 74  J | 75  K | 76  L | 77  M | 78  N | 79  O |
       | 80  P | 81  Q | 82  R | 83  S | 84  T | 85  U | 86  V | 87  W |
       | 88  X | 89  Y | 90  Z | 91  [ | 92  \\ | 93  ] | 94  ^ | 95  _ |
       | 96  ` | 97  a | 98  b | 99  c |100  d |101  e |102  f |103  g |
       |104  h |105  i |106  j |107  k |108  l |109  m |110  n |111  o |
       |112  p |113  q |114  r |115  s |116  t |117  u |118  v |119  w |
       |120  x |121  y |122  z |123  { |124  | |125  } |126  ~ |127 DEL|

       Hexadecimal - Character

       | 00 NUL| 01 SOH| 02 STX| 03 ETX| 04 EOT| 05 ENQ| 06 ACK| 07 BEL|
       | 08 BS | 09 HT | 0A NL | 0B VT | 0C NP | 0D CR | 0E SO | 0F SI |
       | 10 DLE| 11 DC1| 12 DC2| 13 DC3| 14 DC4| 15 NAK| 16 SYN| 17 ETB|
       | 18 CAN| 19 EM | 1A SUB| 1B ESC| 1C FS | 1D GS | 1E RS | 1F US |
       | 20 SP | 21  ! | 22  \" | 23  # | 24  $ | 25  % | 26  & | 27  ' |
       | 28  ( | 29  ) | 2A  * | 2B  + | 2C  , | 2D  - | 2E  . | 2F  / |
       | 30  0 | 31  1 | 32  2 | 33  3 | 34  4 | 35  5 | 36  6 | 37  7 |
       | 38  8 | 39  9 | 3A  : | 3B  ; | 3C  < | 3D  = | 3E  > | 3F  ? |
       | 40  @ | 41  A | 42  B | 43  C | 44  D | 45  E | 46  F | 47  G |
       | 48  H | 49  I | 4A  J | 4B  K | 4C  L | 4D  M | 4E  N | 4F  O |
       | 50  P | 51  Q | 52  R | 53  S | 54  T | 55  U | 56  V | 57  W |
       | 58  X | 59  Y | 5A  Z | 5B  [ | 5C  \\ | 5D  ] | 5E  ^ | 5F  _ |
       | 60  ` | 61  a | 62  b | 63  c | 64  d | 65  e | 66  f | 67  g |
       | 68  h | 69  i | 6A  j | 6B  k | 6C  l | 6D  m | 6E  n | 6F  o |
       | 70  p | 71  q | 72  r | 73  s | 74  t | 75  u | 76  v | 77  w |
       | 78  x | 79  y | 7A  z | 7B  { | 7C  | | 7D  } | 7E  ~ | 7F DEL|

       Octal - Character

       |000 NUL|001 SOH|002 STX|003 ETX|004 EOT|005 ENQ|006 ACK|007 BEL|
       |010 BS |011 HT |012 NL |013 VT |014 NP |015 CR |016 SO |017 SI |
       |020 DLE|021 DC1|022 DC2|023 DC3|024 DC4|025 NAK|026 SYN|027 ETB|
       |030 CAN|031 EM |032 SUB|033 ESC|034 FS |035 GS |036 RS |037 US |
       |040 SP |041  ! |042  \" |043  # |044  $ |045  % |046  & |047  ' |
       |050  ( |051  ) |052  * |053  + |054  , |055  - |056  . |057  / |
       |060  0 |061  1 |062  2 |063  3 |064  4 |065  5 |066  6 |067  7 |
       |070  8 |071  9 |072  : |073  ; |074  < |075  = |076  > |077  ? |
       |100  @ |101  A |102  B |103  C |104  D |105  E |106  F |107  G |
       |110  H |111  I |112  J |113  K |114  L |115  M |116  N |117  O |
       |120  P |121  Q |122  R |123  S |124  T |125  U |126  V |127  W |
       |130  X |131  Y |132  Z |133  [ |134  \\ |135  ] |136  ^ |137  _ |
       |140  ` |141  a |142  b |143  c |144  d |145  e |146  f |147  g |
       |150  h |151  i |152  j |153  k |154  l |155  m |156  n |157  o |
       |160  p |161  q |162  r |163  s |164  t |165  u |166  v |167  w |
       |170  x |171  y |172  z |173  { |174  | |175  } |176  ~ |177 DEL|
       "
  "The ASCII character tables.")

(defconst amory-datetime-formats
  "See:

  * http://pleac.sourceforge.net/pleac_python/datesandtimes.html
  * http://docs.python.org/library/time.html
  * http://docs.python.org/library/datetime.html
  * http://www.python.org/doc/2.5.2/lib/datetime-tzinfo.html
  * http://uswaretech.com/blog/2009/02/understanding-datetime-tzinfo-timedelta-timezone-conversions-python/

  From http://docs.python.org/library/time.html#time.strftime:

    %a      Locale's abbreviated weekday name.
    %A      Locale's full weekday name.
    %b      Locale's abbreviated month name.
    %B      Locale's full month name.
    %c      Locale's appropriate date and time representation.
    %d      Day of the month as a decimal number [01,31].
    %H      Hour (24-hour clock) as a decimal number [00,23].
    %I      Hour (12-hour clock) as a decimal number [01,12].
    %j      Day of the year as a decimal number [001,366].
    %m      Month as a decimal number [01,12].
    %M      Minute as a decimal number [00,59].
    %p      Locale's equivalent of either AM or PM. (1)
    %S      Second as a decimal number [00,61].     (2)
    %U      Week number of the year (Sunday as the first day of the week)
	    as a decimal number [00,53]. All days in a new year preceding
	    the first Sunday are considered to be in week 0.  (3)
    %w      Weekday as a decimal number [0(Sunday),6].
    %W      Week number of the year (Monday as the first day of the week)
	    as a decimal number [00,53]. All days in a new year preceding
	    the first Monday are considered to be in week 0.  (3)
    %x      Locale's appropriate date representation.
    %X      Locale's appropriate time representation.
    %y      Year without century as a decimal number [00,99].
    %Y      Year with century as a decimal number.
    %Z      Time zone name (no characters if no time zone exists).
    %%      A literal '%' character.

    Notes:

      1) When used with the strptime() function, the %p directive only
	 affects the output hour field if the %I directive is used to
	 parse the hour.
      2) The range really is 0 to 61; this accounts for leap seconds
	 and the (very rare) double leap seconds.
      3) When used with the strptime() function, %U and %W are only
	 used in calculations when the day of the week and the year
	 are specified.

    Here is an example, a format for dates compatible with that
    specified in the RFC 2822 Internet email standard. [1]

      >>> from time import gmtime, strftime
      >>> strftime('%a, %d %b %Y %H:%M:%S +0000', gmtime())
      'Thu, 28 Jun 2001 14:17:15 +0000'

== Date codes, for NS (NextStep) Foundation Classes  ==
==   (and possibly the Unix date command as well)    ==

   %a     abbreviated weekday name
   %A     full weekday name
   %b     abbreviated month name
   %B     full month name
   %c     shorthand for %X %x, the locale format for date and time
   %d     day of the month as a decimal number (01-31)
   %e     same as %d but does not print the leading 0 for days 1 through 9
   %F     milliseconds as a decimal number (000 - 999)
   %H     hour based on a 24-hour clock as a decimal number (00-23)
   %I     hour based on a 12-hour clock as a decimal number (01-12)
   %j     day of the year as a decimal number (001-366)
   %m     month as a decimal number (01-12)
   %M     minute as a decimal number (00-59)
   %p     AM/PM designation for the locale
   %S     second as a decimal number (00-61)
   %w     weekday as a decimal number (0-6), where Sunday is 0
   %x     date using the date representation for the locale
   %X     time using the time representation for the locale
   %y     year without century (00-99)
   %Y     year with century (such as 1990)
   %Z     time zone abbreviation (such as PDT)
   %z     time zone offset in hours and minutes from GMT (HHMM)
   %%     a '%' character, of course
"
  "Date and time formats for various programming languages.")

(defconst amory-radio-alphabet
  "A - Alpha                  N - November
B - Bravo                  O - Oscar
C - Charlie                P - Papa
D - Delta                  Q - Quebec
E - Echo                   R - Romeo
F - Foxtrot                S - Sierra
G - Golf                   T - Tango
H - Hotel                  U - Uniform
I - India                  V - Victor
J - Juliet                 W - Whiskey
K - Kilo                   X - X-ray
L - Lima                   Y - Yankee
M - Mike                   Z - Zulu"
  "Wear aviator goggles when confirming airline reservation numbers.")

(defconst amory-stellar-statistics
  "The Sun:
   diameter:    1,390,000 km.
   mass:        1.989e30 kg
   temperature: 5800 K (surface), 15,600,000 K (core)
   ---------------------------------------------------------------------

	       Distance  Radius    Mass
   Planet      (000 km)   (km)     (kg)   Discoverer   Date
   ---------  ---------  ------  -------  ----------  -----
   Mercury       57,910    2439  3.30e23
   Venus        108,200    6052  4.87e24
   Earth        149,600    6378  5.98e24
   Mars         227,940    3397  6.42e23
   Jupiter      778,330   71492  1.90e27
   Saturn     1,426,940   60268  5.69e26
   Uranus     2,870,990   25559  8.69e25   Herschel    1781
   Neptune    4,497,070   24764  1.02e26   Galle       1846


   Non-Planet   (000 km)   (km)     (kg)   Discoverer   Date
   ---------  ---------  ------  -------  ----------  -----
   Pluto      5,913,520    1160  1.31e22   Tombaugh    1930
   ---------------------------------------------------------------------

   So Earth is about 6 septillion kg (5.98 x 10^24 kg).


   No.  Name      Distance  Radius     Mass  Discoverer   Date
   ---- ---------  --------  ------  -------  ----------  -----
   2062 Aten         144514       0.5   ?      Helin       1976
   3554 Amun         145710       ?     ?      Shoemaker   1986
   1566 Icarus       161269       0.7   ?      Baade       1949
    951 Gaspra       205000       8     ?      Neujmin     1916
   1862 Apollo       220061       0.7   ?      Reinmuth    1932
    243 Ida          270000      35     ?      ?           1880?
   2212 Hephaistos   323884       4.4   ?      Chernykh    1978
      4 Vesta        353400     265  3.0e20    Olbers      1807
      3 Juno         399400     123     ?      Harding     1804
     15 Eunomia      395500     136     ?      De Gasparis 1851
      1 Ceres        413900     466  8.7e20    Piazzi      1801
      2 Pallas       414500     261  3.18e20   Olbers      1802
     52 Europa       463300     156     ?      Goldschmidt 1858
     10 Hygiea       470300     215     ?      De Gasparis 1849
    511 Davida       475400     168     ?      Dugan       1903
    911 Agamemnon    778100      88     ?      Reinmuth    1919
   2060 Chiron      2051900      85     ?      Kowal       1977
   ---------------------------------------------------------------------
"
  "Stats on the Sun, planets and selected asteroids.")



;;; genetic code stuff

(defconst amory-genetic-code
  "
 UUU = F  CUU = L  AUU = I  GUU = V    UCU = S  CCU = P  ACU = T  GCU = A
 UUC = F  CUC = L  AUC = I  GUC = V    UCC = S  CCC = P  ACC = T  GCC = A
 UUA = L  CUA = L  AUA = I  GUA = V    UCA = S  CCA = P  ACA = T  GCA = A
 UUG = L  CUG = L  AUG = M  GUG = V    UCG = S  CCG = P  ACG = T  GCG = A

 UAU = Y  CAU = H  AAU = N  GAU = D    UGU = C  CGU = R  AGU = S  GGU = G
 UAC = Y  CAC = H  AAC = N  GAC = D    UGC = C  CGC = R  AGC = S  GGC = G
 UAA = *  CAA = Q  AAA = K  GAA = E    UGA = *  CGA = R  AGA = R  GGA = G
 UAG = *  CAG = Q  AAG = K  GAG = E    UGG = W  CGG = R  AGG = R  GGG = G
 "
  "The genetic code: nucleotides -> amino acids.")


;; Display what we want
(defun amory-display-something-big (contents &optional title)
  "Display string CONTENTS in a buffer named TITLE."
  (let ((buf (get-buffer-create (or title "*STUFF*"))))
    (with-current-buffer buf
      (erase-buffer)
      (insert contents)
      (goto-char (point-min)))
    (display-buffer buf)))

(defmacro amory-gen-displayer (txt-sym fn-doc-str buf-name &optional fn-alias)
  "Generate an interactive function with the same symbol name as TXT-SYM,
whose doc string is FN-DOC-STR, and that when invoked displays TXT-SYM
in a buffer named BUF-NAME using `display-buffer'."
  (declare (indent 2))
  `(progn
     (defun ,txt-sym ()
       ,fn-doc-str
       (interactive)
       (amory-display-something-big ,txt-sym ,buf-name))
     (when (or (not (boundp ',fn-alias)) (not (eq nil ,fn-alias)))
       (defalias ',fn-alias ',txt-sym))))

;; Individual functions for the individual tables
(amory-gen-displayer amory-ascii
    "Display the ASCII character table in its own buffer."
  "*ASCII*")

(amory-gen-displayer amory-datetime-formats
    "Display date/time format codes in their own buffer"
  "*Date / Time Formats*")

(amory-gen-displayer amory-radio-alphabet
    "Display the radio alphabet in its own buffer."
  "*RADIO ALPHABET*")

(amory-gen-displayer amory-stellar-statistics
    "Display some statistics about the solar system."
  "*Solar System*")

(amory-gen-displayer amory-latin-abbreviation-help
    "Display some common lation abbreviations."
  "*Latin*")

(amory-gen-displayer amory-genetic-code
    "Display the genetic code in its own buffer."
  "*THE GENETIC CODE*")

(amory-gen-displayer amory-recentf-help
    "Cheats for opening files, buffer, and rrecently used files and buffer."
  "*Open shit*")


;; BUSTED FIXME TODO ;;;;;;;;;;;;;; ##########
;; Weird, not really sure how to use
(defconst gene-trans-triplet-table
  (list
   ;; On each line/col, the pattern runs UCAG, with U translated to T.

   (list                                ; leftmost U
    ?T (list ?T (cons ?T ?F) (cons ?C ?F) (cons ?A ?L) (cons ?G ?L)) ; mid U
    (list ?C (cons ?T ?S) (cons ?C ?S) (cons ?A ?S) (cons ?G ?S)) ; mid C
    (list ?A (cons ?T ?Y) (cons ?C ?Y) (cons ?A ?*) (cons ?G ?*)) ; mid A
    (list ?G (cons ?T ?C) (cons ?C ?C) (cons ?A ?*) (cons ?G ?W))) ; mid G
   (list                                ; leftmost C
    ?C (list ?T (cons ?T ?L) (cons ?C ?L) (cons ?A ?L) (cons ?G ?L)) ; mid U
    (list ?C (cons ?T ?P) (cons ?C ?P) (cons ?A ?P) (cons ?G ?P)) ; mid C
    (list ?A (cons ?T ?H) (cons ?C ?H) (cons ?A ?Q) (cons ?G ?Q)) ; mid A
    (list ?G (cons ?T ?R) (cons ?C ?R) (cons ?A ?R) (cons ?G ?R))) ; mid G
   (list                                ; leftmost A
    ?A (list ?T (cons ?T ?I) (cons ?C ?I) (cons ?A ?I) (cons ?G ?M)) ; mid U
    (list ?C (cons ?T ?T) (cons ?C ?T) (cons ?A ?T) (cons ?G ?T)) ; mid C
    (list ?A (cons ?T ?N) (cons ?C ?N) (cons ?A ?K) (cons ?G ?K)) ; mid A
    (list ?G (cons ?T ?S) (cons ?C ?S) (cons ?A ?R) (cons ?G ?R))) ; mid G
   (list                                ; leftmost G
    ?G (list ?T (cons ?T ?V) (cons ?C ?V) (cons ?A ?V) (cons ?G ?V)) ; mid U
    (list ?C (cons ?T ?A) (cons ?C ?A) (cons ?A ?A) (cons ?G ?A)) ; mid C
    (list ?A (cons ?T ?D) (cons ?C ?D) (cons ?A ?E) (cons ?G ?E)) ; mid A
    (list ?G (cons ?T ?G) (cons ?C ?G) (cons ?A ?G) (cons ?G ?G)))) ; mid G
  "Table for translating nucleotide triplets into amino acids.")


(defun gene-trans-triplet-to-amino-internal (ch1 ch2 ch3)
  "Translate the triplet CH1 CH2 CH3 to an amino acid character.
Case-sensitive, and only handles T, not U.
 Returns nil if no such triplet code.
 You probably don't want to use this function.  Take a look at
 `gene-trans-triplet-to-amino' instead."
  (cdr (assoc
	ch3
	(cdr (assoc
	      ch2
	      (cdr (assoc ch1 gene-trans-triplet-table)))))))


(defun gene-trans-triplet-to-amino (ch1 ch2 ch3)
  "Translate the triplet CH1 CH2 CH3 to an amino acid character.
Case-insensitive.  U or T may be used interchangably.
 If the triplet does not code for anything, return `X'.
 The input characters are three separate arguments, not a list."
  (or
   (gene-trans-triplet-to-amino-internal
    (min (upcase ch1) ?T) (min (upcase ch2) ?T) (min (upcase ch3) ?T))
   ?X))

;; Appears to have an off-by-one-error at the very end of the
;; translation, hmm.
(defun amory-gene-translate-region (b e)
  "Interpret region from B to E as nucleotides, insert the
corresponding amino acids before B, followed by a newline."
  (interactive "r")
  (save-excursion
    (goto-char b)
    (setq e (copy-marker (- e (mod (- e b) 3))))
    (let ((insert-pt (point))
	  (trans-pt (progn (re-search-forward "[a-zA-Z]")
			   (forward-char -1)
			   (point))))
      (while (<= trans-pt e)
	(let ((char1 (char-after))
	      (char2 (progn (forward-char 1) (char-after)))
	      (char3 (progn (forward-char 1) (char-after))))
	  (setq trans-pt (point))
	  (goto-char insert-pt)
	  ;; Notice the o-b-o-e.
	  (if (null char1) (error "Char1 is nil, %d" trans-pt))
	  (if (null char2) (error "Char2 is nil, %d" trans-pt))
	  (if (null char3) (error "Char3 is nil, %d" trans-pt))
	  (insert (gene-trans-triplet-to-amino char1 char2 char3))
	  (setq insert-pt (point))
	  (goto-char trans-pt)))
      (goto-char insert-pt)
      (insert "\n"))))

;;; end genetic code stuff


(provide 'amory-reference-functions)

;;; amory-reference-functions.el ends here
