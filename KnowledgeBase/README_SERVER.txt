g4t.itd.cnr.it - virt6itd.itd.cnr.it
vecchie credenziali: sviluppo/FerraGosto_itd
nuove credenziali: luigi/Gn1RbAW1bg\)z!bNH

Alla partenza del server:
luigi@virt6itd:~$ cd Gioco_4T/KnowledgeBase
luigi@virt6itd:~/Gioco_4T/KnowledgeBase$ screen ./Autostart.sh

to detach a screen session: "Ctrl-a d"

to reattach from shell: screen  -r 10609.pts-0.sviluppo-Aspire-M3970
where the PID should be obtained with:

> screen -ls
There is a screen on:
        10609.pts-0.sviluppo-Aspire-M3970       (19/08/2019 14:38:14)   (Detached)
1 Socket in /run/screen/S-sviluppo.


if the previous screen had not been correctly detached:
> screen  -d -r 10609.pts-0.sviluppo-Aspire-M3970


Running the SWI-pl server from Linux upstart
http://www.swi-prolog.org/howto/http/UpStart.txt

