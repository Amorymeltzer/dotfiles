#!/usr/bin/env perl

# Faithful-ish Perl implementation of lexers.rb by Brett Terpstra:
# https://gist.github.com/ttscoff/14560a71b86a48b4d895087d3700f516
# This script located at:
# https://gist.github.com/Amorymeltzer/6f050ad73f2b6a98c9c8de71f253714f

use strict;
use warnings;

use Getopt::Long;
use File::Basename;

use 5.010;

my %opts = ();
GetOptions(\%opts,
  'e|ext-for=s',
  'l|lang-for=s',
  'h|help'
);

my @lexers;
sub buildLexers {
  while (my $line = <DATA>) {
    chomp $line;

    next if ! $line;
    my ($lxr) = $line =~ /^((?:(?:, )?[^,]+?)+?)(?=<)/mi;
    my @lxrs = map { strip($_) } split /,/, $lxr;


    my ($title) = $line =~ /<(.*?)>/;

    my ($ext) = $line =~ /\[(.*?)\]/;
    my @exts = map { strip($_) } split /,/, $ext;

    push @lexers, {
		   title => $title,
		   lexer => shift @lxrs,
		   aliases => \@lxrs,
		   extensions => \@exts
		  };
  }
}

# Recapitulate ruby's .strip
sub strip {
  return $_[0] =~ s/^\s+|\s+$//gr;
}

sub normalize {
  return lc $_[0] =~ s/[^a-z0-9]//gir;
}

sub langForExt {
  my $ext = normalize shift;

  my @matches = grep {
    ((grep {normalize($_) eq $ext} @{${$_}{extensions}}) || (normalize(${$_}{lexer}) eq $ext) || (grep {normalize($_) eq $ext} @{${$_}{aliases}}))
  } @lexers;

  my $output = join "\n\n", map {
    my $lex = $_;
    my $o = "${$lex}{title}\n";
    $o .= "lexers: ${$lex}{lexer}";
    $o .= ', ' . join ', ', @{${$lex}{aliases}} if @{${$lex}{aliases}};
    $o .= "\n";
    $o .= scalar @{${$lex}{extensions}}
      ? 'extensions: ' . join ', ', map { "*.$_" } @{${$lex}{extensions}}
      : "extension: *.${${$lex}{extensions}}[0]";
    $o;
  } @matches;
}

# Includes some, like ANTLR with Perl? FIXME
sub extForLang {
  my $lexer = normalize shift;

  my @matches = grep {
    ((${$_}{title} =~ /\b$lexer/i || ${$_}{lexer} =~ /\b$lexer/i || grep { $_ eq $lexer } map { normalize($_) eq $lexer } @{${$_}{aliases}}))
  } @lexers;

  join "\n", map { "${$_}{title} extensions: " . join ', ', @{${$_}{extensions}} } @matches;
}

buildLexers();

# Technically, lexers.rb goes through the args individually and in order
if ($opts{l}) {
  say langForExt($opts{l});
  exit;
}
if ($opts{e}) {
  say extForLang($opts{e});
  exit;
}
if ($opts{h}) {
  my $name = basename(__FILE__);
  print <<"USAGE";
Usage: $name [-l EXTENSION|-e LANGUAGE|ARGUMENTS]
    -e, --ext-for=LANGUAGE           Get the most common extensions for a langauge
    -l, --lang-for=EXTENSION         Get a language key for an extension
    -h, --help                       Display this screen
USAGE
  exit;
}


__END__
abap <ABAP> [abap, ABAP]
abc <ABC> []
abnf <ABNF> [abnf]
actionscript, as <ActionScript> [as]
actionscript3, as3 <ActionScript 3> [as]
ada, ada95, ada2005 <Ada> [adb, ads, ada]
adl <ADL> [adl, adls, adlf, adlx]
agda <Agda> [agda]
aheui <Aheui> [aheui]
alert <Alerts> []
alloy <Alloy> [als]
ambienttalk, ambienttalk/2, at <AmbientTalk> [at]
amdgpu <AMDGPU> [isa]
ampl <Ampl> [run]
ansys, apdl <ANSYS parametric design language> [ans]
antlr <ANTLR> []
antlr-actionscript, antlr-as <ANTLR With ActionScript Target> [G, g]
antlr-cpp <ANTLR With CPP Target> [G, g]
antlr-csharp, antlr-c# <ANTLR With C# Target> [G, g]
antlr-java <ANTLR With Java Target> [G, g]
antlr-objc <ANTLR With ObjectiveC Target> [G, g]
antlr-perl <ANTLR With Perl Target> [G, g]
antlr-python <ANTLR With Python Target> [G, g]
antlr-ruby, antlr-rb <ANTLR With Ruby Target> [G, g]
apacheconf, aconf, apache <ApacheConf> [.htaccess, apache.conf, apache2.conf]
apl <APL> [apl, aplf, aplo, apln, aplc, apli, dyalog]
applescript <AppleScript> [applescript]
arduino <Arduino> [ino]
arrow <Arrow> [arw]
asc, pem <ASCII armored> [asc, pem, id_dsa, id_ecdsa, id_ecdsa_sk, id_ed25519, id_ed25519_sk, id_rsa]
asn1 <ASN.1> []
asp <ASP> []
aspectj <AspectJ> [aj]
aspx-cs <aspx-cs> [aspx, asax, ascx, ashx, asmx, axd]
aspx-vb <aspx-vb> [aspx, asax, ascx, ashx, asmx, axd]
asymptote, asy <Asymptote> [asy]
ats <ATS> []
augeas <Augeas> [aug]
autohotkey, ahk <autohotkey> [ahk, ahkl]
autoit <AutoIt> [au3]
awk, gawk, mawk, nawk <Awk> [awk]
bare <BARE> [bare]
basemake <Base Makefile> []
bash, sh, ksh, zsh, shell <Bash> [sh, ksh, bash, ebuild, eclass, exheres-0, exlib, zsh, .bashrc, bashrc, .bash_*, bash_*, zshrc, .zshrc, PKGBUILD]
batch, bat, dosbatch, winbatch <Batchfile> [bat, cmd]
bbcbasic <BBC Basic> [bbc]
bbcode <BBCode> []
bc <BC> [bc]
befunge <Befunge> [befunge]
bibtex, bib <BibTeX> [bib]
blitzbasic, b3d, bplus <BlitzBasic> [bb, decls]
blitzmax, bmax <BlitzMax> [bmx]
bnf <BNF> [bnf]
boa <Boa> [boa]
boo <Boo> [boo]
boogie <Boogie> [bpl]
brainfuck, bf <Brainfuck> [bf, b]
bst, bst-pybtex <BST> [bst]
bugs, winbugs, openbugs <BUGS> [bug]
c <C> [c, h, idc]
c-objdump <c-objdump> [c-objdump]
ca65 <ca65 assembler> [s]
cadl <cADL> [cadl]
camkes, idl4 <CAmkES> [camkes, idl4]
capdl <CapDL> [cdl]
capnp <Cap'n Proto> [capnp]
cbmbas <CBM BASIC V2> [bas]
cddl <CDDL> [cddl]
ceylon <Ceylon> [ceylon]
cfc <Coldfusion CFC> [cfc]
cfengine3, cf3 <CFEngine3> [cf]
cfm <Coldfusion HTML> [cfm, cfml]
cfs <cfstatement> []
chaiscript, chai <ChaiScript> [chai]
changelog <ChangeLog> []
chapel, chpl <Chapel> [chpl]
charmci <Charmci> [ci]
cheetah, spitfire <Cheetah> [tmpl, spt]
cirru <Cirru> [cirru]
clay <Clay> [clay]
clean <Clean> [icl, dcl]
clojure, clj <Clojure> [clj]
clojurescript, cljs <ClojureScript> [cljs]
cmake <CMake> [cmake, CMakeLists.txt]
cobol <COBOL> [cob, COB, cpy, CPY]
cobolfree <COBOLFree> [cbl, CBL]
coffeescript, coffee-script, coffee <CoffeeScript> [coffee]
coldfusion <ColdFusion> []
comments <Comments> []
common-lisp, commonlisp, cl, lisp <Common Lisp> [cl, lisp]
componentpascal, cp <Component Pascal> [cp, cps]
console, shell-session <Bash Session> [sh-session, shell-session]
coq <Coq> [v]
cpp, c++ <C++> [cpp, hpp, c++, h++, cc, hh, cxx, hxx, C, H, cp, CPP]
cpp-objdump, c++-objdumb, cxx-objdump <cpp-objdump> [cpp-objdump, c++-objdump, cxx-objdump]
cpsa <CPSA> [cpsa]
cr, crystal <Crystal> [cr]
crmsh, pcmk <Crmsh> [crmsh, pcmk]
croc <Croc> [croc]
cryptol, cry <Cryptol> [cry]
csharp, c#, cs <C#> [cs]
csound, csound-orc <Csound Orchestra> [orc, udo]
csound-document, csound-csd <Csound Document> [csd]
csound-score, csound-sco <Csound Score> [sco]
css <CSS> [css]
css+django, css+jinja <CSS+Django/Jinja> []
css+genshitext, css+genshi <CSS+Genshi Text> []
css+lasso <CSS+Lasso> []
css+mako <CSS+Mako> []
css+mozpreproc <CSS+mozpreproc> [css.in]
css+myghty <CSS+Myghty> []
css+php <CSS+PHP> []
css+ruby, css+erb <CSS+Ruby> []
css+smarty <CSS+Smarty> []
cuda, cu <CUDA> [cu, cuh]
curry <Curry> []
cypher <Cypher> [cyp, cypher]
cython, pyx, pyrex <Cython> [pyx, pxd, pxi]
d <D> [d, di]
d-objdump <d-objdump> [d-objdump]
dart <Dart> [dart]
dasm16 <DASM16> [dasm16, dasm]
debcontrol, control <Debian Control file> [control]
debsources, sourceslist, sources.list <Debian Sourcelist> [sources.list]
default <Default> []
delphi, pas, pascal, objectpascal <Delphi> [pas, dpr]
devicetree, dts <Devicetree> [dts, dtsi]
dg <dg> [dg]
diff, udiff <Diff> [diff, patch]
django, jinja <Django/Jinja> []
djangotemplate <Django HTML Template> []
docker, dockerfile <Docker> [Dockerfile, docker]
doscon <MSDOS Session> []
dot <dot> []
doxygen <Doxygen> []
doxygenlua <DoxygenLua> []
dpatch <Darcs Patch> [dpatch, darcspatch]
dtd <DTD> [dtd]
duel, jbst, jsonml+bst <Duel> [duel, jbst]
dylan <Dylan> [dylan, dyl, intr]
dylan-console, dylan-repl <Dylan session> [dylan-console]
dylan-lid, lid <DylanLID> [lid, hdp]
earl-grey, earlgrey, eg <Earl Grey> [eg]
easytrieve <Easytrieve> [ezt, mac]
ebnf <EBNF> [ebnf]
ec <eC> [ec, eh]
ecl <ECL> [ecl]
eiffel <Eiffel> [e]
elixir, ex, exs <Elixir> [ex, eex, exs, leex]
elm <Elm> [elm]
emacs-lisp, elisp, emacs <EmacsLisp> [el]
email, eml <E-mail> [eml]
erb <ERB> [erb]
erl <Erlang erl session> [erl-sh]
erlang <Erlang> [erl, hrl, es, escript]
evoque <Evoque> [evoque]
execline <execline> [exec]
extempore <xtlang> [xtm]
ezhil <Ezhil> [n]
factor <Factor> [factor]
fan <Fantom> [fan]
fancy, fy <Fancy> [fy, fancypack]
fasm <Intel x86 (FASM> [])
felix, flx <Felix> [flx, flxh]
fennel, fnl <Fennel> [fnl]
fish, fishshell <Fish> [fish, load]
flatline <Flatline> []
floscript, flo <FloScript> [flo]
forth <Forth> [frt, fs]
fortran <Fortran> [f03, f90, F03, F90]
fortranfixed <FortranFixed> [f, F]
fortranfree <Fortran (Free Format)> []
foxpro, vfp, clipper, xbase <FoxPro> [PRG, prg]
freefem <Freefem> [edp]
fsharp, f# <F#> [fs, fsi]
fstar <FStar> [fst, fsti]
futhark <Futhark> [fut]
gap <GAP> [g, gd, gi, gap]
gas, asm <GAS> [s, S]
gcc <GCCExtensions> []
gcode <g-code> [gcode]
gdscript, gd <GDScript> [gd]
genshi, kid, xml+genshi, xml+kid <Genshi> [kid]
genshitext <Genshi Text> []
gherkin, cucumber <Gherkin> [feature]
glsl <GLSL> [vert, frag, geo]
gnuassembler <GNU Assembler> []
gnuplot <Gnuplot> [plot, plt]
go, golang <Go> [go]
golo <Golo> [golo]
gooddata-cl <GoodData-CL> [gdc]
gosu <Gosu> [gs, gsx, gsp, vark]
graphql <GraphQL> []
graphviz, dot <Graphviz> [gv, dot]
groff, nroff, man <Groff> [[1234567], man]
groovy <Groovy> [groovy, gradle]
gsql <GSQL> [gsql]
gst <Gosu Template> [gst]
haml <Haml> [haml]
hamlet <Hamlet> []
handlebars <Handlebars> []
haskell, hs <Haskell> [hs]
haxe, hxsl, hx <Haxe> [hx, hxsl]
haxeml, hxml <Hxml> [hxml]
hexdump <Hexdump> []
hlsl <HLSL> [hlsl, hlsli]
hsail, hsa <HSAIL> [hsail]
hspec <Hspec> []
html <HTML> [html, htm, xhtml, xslt]
html+cheetah, html+spitfire, htmlcheetah <HTML+Cheetah> []
html+django, html+jinja, htmldjango <HTML+Django/Jinja> []
html+evoque <HTML+Evoque> [html]
html+genshi, html+kid <HTML+Genshi> []
html+handlebars <HTML+Handlebars> [handlebars, hbs]
html+lasso <HTML+Lasso> []
html+mako <HTML+Mako> []
html+myghty <HTML+Myghty> []
html+ng2 <HTML + Angular2> [ng2]
html+php <HTML+PHP> [phtml]
html+smarty <HTML+Smarty> []
html+twig <HTML+Twig> [twig]
html+velocity <HTML+Velocity> []
http <HTTP> []
hybris, hy <Hybris> [hy, hyb]
hylang <Hy> [hy]
i6t <Inform 6 template> [i6t]
icon <Icon> [icon, ICON]
idl <IDL> [pro]
idris, idr <Idris> [idr]
iex <Elixir iex session> []
igor, igorpro <Igor> [ipf]
inform6, i6 <Inform 6> [inf]
inform7, i7 <Inform 7> [ni, i7x]
ini, cfg, dosini <INI> [ini, cfg, inf, service, socket, device, mount, automount, swap, target, path, timer, slice, scope]
io <Io> [io]
ioke, ik <Ioke> [ik]
irc <IRC logs> [weechatlog]
isabelle <Isabelle> [thy]
isocpp <ISO C++> []
j <J> [ijs]
jags <JAGS> [jag, bug]
jasmin, jasminxt <Jasmin> [j]
java <Java> [java]
javadoc <Javadoc> []
javascript+cheetah, js+cheetah, javascript+spitfire, js+spitfire <JavaScript+Cheetah> []
javascript+django, js+django, javascript+jinja, js+jinja <JavaScript+Django/Jinja> []
javascript+lasso, js+lasso <JavaScript+Lasso> []
javascript+mako, js+mako <JavaScript+Mako> []
javascript+mozpreproc <Javascript+mozpreproc> [js.in]
javascript+myghty, js+myghty <JavaScript+Myghty> []
javascript+php, js+php <JavaScript+PHP> []
javascript+ruby, js+ruby, javascript+erb, js+erb <JavaScript+Ruby> []
javascript+smarty, js+smarty <JavaScript+Smarty> []
javascript, js <JavaScript> [js, jsm, mjs, cjs]
javascriptreact <JavaScript React (JSX> [])
jcl <JCL> [jcl]
jlcon, julia-repl <Julia console> []
js+genshitext, js+genshi, javascript+genshitext, javascript+genshi <JavaScript+Genshi Text> []
jsgf <JSGF> [jsgf]
jslt <JSLT> [jslt]
json, json-object <JSON> [json, Pipfile.lock]
jsonld, json-ld <JSON-LD> [jsonld]
jsp <Java Server Page> [jsp]
julia, jl <Julia> [jl]
juttle <Juttle> [juttle]
kal <Kal> [kal]
kconfig, menuconfig, linux-config, kernel-config <Kconfig> [Kconfig*, *Config.in*, external.in*, standard-modules.in]
kmsg, dmesg <Kernel log> [kmsg, dmesg]
koka <Koka> [kk, kki]
kotlin <Kotlin> [kt, kts]
kuin <Kuin> [kn]
lasso, lassoscript <Lasso> [lasso, lasso[89]]
latex <LaTeX> []
lean <Lean> [lean]
less <LessCss> [less]
lex <Lex/Flex> []
lighttpd, lighty <Lighttpd configuration file> [lighttpd.conf]
lilypond <LilyPond> []
limbo <Limbo> [b]
liquid <liquid> [liquid]
literate-agda, lagda <Literate Agda> [lagda]
literate-cryptol, lcryptol, lcry <Literate Cryptol> [lcry]
literate-haskell, lhaskell, lhs <Literate Haskell> [lhs]
literate-idris, lidris, lidr <Literate Idris> [lidr]
literatecurry <Literate Curry> []
literatehaskell <Literate Haskell> []
livescript, live-script <LiveScript> [ls]
llvm <LLVM> [ll]
llvm-mir <LLVM-MIR> [mir]
llvm-mir-body <LLVM-MIR Body> []
logos <Logos> [x, xi, xm, xmi]
logtalk <Logtalk> [lgt, logtalk]
lsl <LSL> [lsl]
lua <Lua> [lua, wlua]
m4 <GNU M4> []
make, makefile, mf, bsdmake <Makefile> [mak, mk, Makefile, makefile, Makefile.*, GNUmakefile]
makefile <Makefile> []
mako <Mako> [mao]
mandoc <Troff Mandoc> []
maql <MAQL> [maql]
markdown, md <Markdown> [md, markdown, mmd]
mask <Mask> [mask]
mason <Mason> [m, mhtml, mc, mi, autohandler, dhandler]
mathematica, mma, nb <Mathematica> [nb, cdf, nbp, ma]
matlab <Matlab> [m]
matlabsession <Matlab session> []
maxima <Maxima> []
mediawiki <MediaWiki> []
meson, meson.build <Meson> [meson.build, meson_options.txt]
metafont <Metapost/Metafont> []
mime <MIME> []
minid <MiniD> []
miniscript, ms <MiniScript> [ms]
mips <MIPS Assembler> []
modelica <Modelica> [mo]
modelines <Modelines> []
modula2, m2 <Modula-2> [def, mod]
modula3 <Modula-3> []
monkey <Monkey> [monkey]
monobasic <MonoBasic> []
monte <Monte> [mt]
moocode, moo <MOOCode> [moo]
moonscript, moon <MoonScript> [moon]
mosel <Mosel> [mos]
mozhashpreproc <mozhashpreproc> []
mozpercentpreproc <mozpercentpreproc> []
mql, mq4, mq5, mql4, mql5 <MQL> [mq4, mq5, mqh]
mscgen, msc <Mscgen> [msc]
mupad <MuPAD> [mu]
mustache <Mustache/Handlebars (HTML)> []
mxml <MXML> [mxml]
myghty <Myghty> [myt, autodelegate]
mysql <MySQL> [sql]
nasm <NASM> [asm, ASM]
ncl <NCL> [ncl]
nemerle <Nemerle> [n]
nesc <nesC> [nc]
nestedtext, nt <NestedText> [nt]
newlisp <NewLisp> [lsp, nl, kif]
newspeak <Newspeak> [ns2]
ng2 <Angular2> []
nginx <Nginx configuration file> [nginx.conf]
nimrod, nim <Nimrod> [nim, nimrod]
nit <Nit> [nit]
nixos, nix <Nix> [nix]
nodejsrepl <Node.js REPL console session> []
notmuch <Notmuch> []
noweb <noweb> []
nsis, nsi, nsh <NSIS> [nsi, nsh]
numpy <NumPy> []
nusmv <NuSMV> [smv]
objdump <objdump> [objdump]
objdump-nasm <objdump-nasm> [objdump-intel]
objective-c++, objectivec++, obj-c++, objc++ <Objective-C++> [mm, hh]
objective-c, objectivec, obj-c, objc <Objective-C> [m, h]
objective-j, objectivej, obj-j, objj <Objective-J> [j]
ocaml <OCaml> [ml, mli, mll, mly]
octave <Octave> [m]
odin <ODIN> [odin]
omg-idl <OMG Interface Definition Language> [idl, pidl]
ooc <Ooc> [ooc]
opa <Opa> [opa]
opencl <OpenCL> []
openedge, abl, progress <OpenEdge ABL> [p, cls]
orgmode <Org Mode> []
output <Text output> []
pacmanconf <PacmanConf> [pacman.conf]
pan <Pan> [pan]
parasail <ParaSail> [psi, psl]
pascal <Pascal> []
pawn <Pawn> [p, pwn, inc]
peg <PEG> [peg]
perl, pl <Perl> [pl, pm, t, perl]
perl6, pl6, raku <Perl6> [pl, pm, nqp, p6, 6pl, p6l, pl6, 6pm, p6m, pm6, t, raku, rakumod, rakutest, rakudoc]
php, php3, php4, php5 <PHP> [php, php[345], inc]
pig <Pig> [pig]
pike <Pike> [pike, pmod]
pkgconfig <PkgConfig> [pc]
plpgsql <PL/pgSQL> []
pointless <Pointless> [ptls]
pony <Pony> [pony]
postgresql, postgres <PostgreSQL SQL dialect> []
postscript <PostScript> []
postscript, postscr <PostScript> [ps, eps]
pot, po <Gettext Catalog> [pot, po]
pov, povray <POVRay> [pov, inc]
powershell <PowerShell> []
powershell, pwsh, posh, ps1, psm1 <PowerShell> [ps1, psm1]
praat <Praat> [praat, proc, psc]
procfile <Procfile> [Procfile]
prolog <Prolog> [ecl, prolog, pro, pl]
promql <PromQL> [promql]
properties, jproperties <Properties> [properties]
protobuf, proto <Protocol Buffer> [proto]
psql, postgresql-console, postgres-console <PostgreSQL console (psql)> []
psysh <PsySH console session for PHP> []
pug, jade <Pug> [pug, jade]
puppet <Puppet> [pp]
pure <Pure> []
purebasic <PureBasic> []
pwsh-session, ps1con <PowerShell Session> []
py2tb <Python 2.x Traceback> [py2tb]
pycon <Python console session> []
pypylog, pypy <PyPy Log> [pypylog]
pytb, py3tb <Python Traceback> [pytb, py3tb]
python, py, sage, python3, py3 <Python> [py, pyw, jy, sage, sc, SConstruct, SConscript, bzl, BUCK, BUILD, BUILD.bazel, WORKSPACE, tac]
python2, py2 <Python 2.x> [py, py2]
qbasic, basic <QBasic> [BAS, bas]
qml, qbs <QML> [qml, qbs]
qvto, qvt <QVTO> [qvto]
r <R Script> []
racket, rkt <Racket> [rkt, rktd, rktl]
ragel <Ragel> []
ragel-c <Ragel in C Host> [rl]
ragel-cpp <Ragel in CPP Host> [rl]
ragel-d <Ragel in D Host> [rl]
ragel-em <Embedded Ragel> [rl]
ragel-java <Ragel in Java Host> [rl]
ragel-objc <Ragel in Objective C Host> [rl]
ragel-ruby, ragel-rb <Ragel in Ruby Host> [rl]
raku <Raku> []
rbcon, irb <Ruby irb session> []
rconsole, rout <RConsole> [Rout]
rd <Rd> [Rd]
reasonml, reason <ReasonML> [re, rei]
rebol <REBOL> [r, r3, reb]
red, red/system <Red> [red, reds]
redcode <Redcode> [cw]
registry <reg> [reg]
relaxng <RELAX NG> []
relaxngcompact <RelaxNG-Compact> []
resourcebundle, resource <ResourceBundle> []
restructuredtext, rst, rest <reStructuredText> [rst, rest]
rexx, arexx <Rexx> [rexx, rex, rx, arexx]
rhtml, html+erb, html+ruby <RHTML> [rhtml]
ride <Ride> [ride]
rng-compact, rnc <Relax-NG Compact> [rnc]
roboconf-graph <Roboconf Graph> [graph]
roboconf-instances <Roboconf Instances> [instances]
robotframework <RobotFramework> [robot]
roff <Roff> []
rql <RQL> [rql]
rsl <RSL> [rsl]
ruby, rb, duby <Ruby> [rb, rbw, Rakefile, rake, gemspec, rbx, duby, Gemfile]
rust, rs <Rust> [rs, rs.in]
sarl <SARL> [sarl]
sas <SAS> [SAS, sas]
sass <Sass> [sass]
scala <Scala> [scala]
scaml <Scaml> [scaml]
scdoc, scd <scdoc> [scd, scdoc]
scheme, scm <Scheme> [scm, ss]
scilab, sci <Scilab> [sci, sce, tst]
scss <SCSS> [scss]
sed <sed> []
sgf <SmartGameFormat> [sgf]
sgml <SGML> []
shen <Shen> [shen]
shexc, shex <ShExC> [shex]
sieve <Sieve> [siv, sieve]
silver <Silver> [sil, vpr]
singularity <Singularity> [def, Singularity]
slash <Slash> [sla]
slim <Slim> [slim]
slurm, sbatch <Slurm> [sl]
smali <Smali> [smali]
smalltalk, squeak, st <Smalltalk> [st]
smarty <Smarty> [tpl]
smithy <Smithy> [smithy]
sml <Standard ML> [sml, sig, fun]
snobol <Snobol> [snobol]
snowball <Snowball> [sbl]
solidity <Solidity> [sol]
sp <SourcePawn> [sp]
sparql <SPARQL> [rq, sparql]
spdxcomments <SPDX-Comments> []
spec <RPMSpec> [spec]
splus, s, r <S> [S, R, .Rhistory, .Rprofile, .Renviron]
sql <SQL> [sql]
sqlite3 <sqlite3con> [sqlite3-console]
sqlmysql <SQL (MySQL)> []
sqlpostgresql <SQL (PostgreSQL)> []
squidconf, squid.conf, squid <SquidConf> [squid.conf]
ssp <Scalate Server Page> [ssp]
stan <Stan> [stan]
stata, do <Stata> [do, ado]
supercollider, sc <SuperCollider> [sc, scd]
swift <Swift> [swift]
swig <SWIG> [swg, i]
systemverilog, sv <systemverilog> [sv, svh]
tads3 <TADS 3> [t]
tap <TAP> [tap]
tasm <TASM> [asm, ASM, tasm]
tcl <Tcl> [tcl, rvt]
tcsh, csh <Tcsh> [tcsh, csh]
tcshcon <Tcsh Session> []
tea <Tea> [tea]
teal <teal> [teal]
teratermmacro, teraterm, ttl <Tera Term macro> [ttl]
termcap <Termcap> [termcap, termcap.src]
terminfo <Terminfo> [terminfo, terminfo.src]
terraform, tf <Terraform> [tf]
tex, latex <TeX> [tex, aux, toc]
texinfo <Texinfo> []
text <Text only> [txt]
thrift <Thrift> [thrift]
ti, thingsdb <ThingsDB> [ti]
tid <tiddler> [tid]
tnt <Typographic Number Theory> [tnt]
todotxt <Todotxt> [todo.txt, todotxt]
toml <TOML> [toml, Pipfile, poetry.lock]
trac-wiki, moin <MoinMoin/Trac Wiki markup> []
trafficscript, rts <TrafficScript> [rts]
treetop <Treetop> [treetop, tt]
tsql, t-sql <Transact-SQL> [sql]
turtle <Turtle> [ttl]
twig <Twig> []
typescript, ts <TypeScript> [ts]
typoscript <TypoScript> [typoscript]
typoscriptcssdata <TypoScriptCssData> []
typoscripthtmldata <TypoScriptHtmlData> []
ucode <ucode> [u, u1, u2]
unicon <Unicon> [icn]
urbiscript <UrbiScript> [u]
usd, usda <USD> [usd, usda]
vala, vapi <Vala> [vala, vapi]
vb.net, vbnet <VB.net> [vb, bas]
vbscript <VBScript> [vbs, VBS]
vcl <VCL> [vcl]
vclsnippets, vclsnippet <VCLSnippets> []
vctreestatus <VCTreeStatus> []
velocity <Velocity> [vm, fhtml]
verilog, v <verilog> [v]
vgl <VGL> [rpf]
vhdl <vhdl> [vhdl, vhd]
vim <VimL> [vim, .vimrc, .exrc, .gvimrc, _vimrc, _exrc, _gvimrc, vimrc, gvimrc]
wast, wat <WebAssembly> [wat, wast]
wdiff <WDiff> [wdiff]
webidl <Web IDL> [webidl]
whiley <Whiley> [whiley]
x10, xten <X10> [x10]
xml <XML> [xml, xsl, rss, xslt, xsd, wsdl, wsf]
xml+cheetah, xml+spitfire <XML+Cheetah> []
xml+django, xml+jinja <XML+Django/Jinja> []
xml+evoque <XML+Evoque> [xml]
xml+lasso <XML+Lasso> []
xml+mako <XML+Mako> []
xml+myghty <XML+Myghty> []
xml+php <XML+PHP> []
xml+ruby, xml+erb <XML+Ruby> []
xml+smarty <XML+Smarty> []
xml+velocity <XML+Velocity> []
xorg, xorg.conf <Xorg> [xorg.conf]
xquery, xqy, xq, xql, xqm <XQuery> [xqy, xquery, xq, xql, xqm]
xslt <XSLT> [xsl, xslt, xpl]
xtend <Xtend> [xtend]
xul <XUL> []
xul+mozpreproc <XUL+mozpreproc> [xul.in]
yacc <Yacc/Bison> []
yaml <YAML> [yaml, yml]
yaml+jinja, salt, sls <YAML+Jinja> [sls]
yang <YANG> [yang]
zeek, bro <Zeek> [zeek, bro]
zephir <Zephir> [zep]
zig <Zig> [zig]
zsh <Zsh> [zsh]
