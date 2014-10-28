#!/usr/bin/env bash
# pokemon.sh by ????
# Tweaked October 2014 by Amory Meltzer to allow stdin and multiple inputs
# Print correct pokemon given a number (original 150 only)
# http://codegolf.stackexchange.com/q/9037/8946

if [ ! "$1" ]; then
    read var;
else
    var=$@;
fi
for n in $var;
do
    echo {Bulba,Ivy,Venu}saur Char{mander,meleon,izard} {Squi,Warto}rtle Blastoise Caterpie Metapod Butterfree Weedle Kakuna Beedrill Pidge{y,otto,ot} Rat{tata,icate} {Sp,F}earow Ekans Arbok {Pika,Rai}chu Sands{hrew,lash} Nido{ran%\(f\),rina,queen,ran%\(m\),rino,king} Clefa{iry,ble} Vulpix Ninetales {Jigglyp,Wigglyt}uff {Zu,Gol}bat Oddish Gloom Vileplume Paras{,ect} Veno{nat,moth} Diglett Dugtrio Meowth Persian {Psy,Gol}duck Mankey Primeape Growlith Arcanine Poliw{ag,hirl,rath} {A,Kada}bra Alakazam Mach{op,oke,amp} Bellsprout {Weepin,Victree}bell Tentac{ool,ruel} Geodude Graveler Golem Ponyta Rapidash Slow{poke,bro} Magne{mite,ton} Farfetch\'d Dod{uo,rio} Seel Dewgong Grimer Muk {Shelld,Cloyst}er Gastly Haunter Gengar Onix Drowsee Hypno Krabby Kingler Voltorb Electrode Exeggut{e,or} Cubone Marowak Hitmon{lee,chan} Lickitung {Koff,Weez}ing Rhy{horn,don} Chansey Tangela Kangaskhan Horsea Seadra Goldeen Seaking Star{yu,mie} Mr.%Mime Scyther Jynx Electabuzz Magmar Pinsir Tauros Magikarp Gyarados Lapras Ditto Eevee {Vapore,Jolte,Flare,Poryg}on Oma{nyte,star} Kabuto{,ps} Aerodactyl Snorlax Articuno Zapdos Moltres Dra{tini,gon{air,ite}} Mew{two,}|tr %\  \ \\n|sed $n!d
done
