game boy emulator in c89

games tested:
* Tetris -> playable
* Wario Land: Super Mario Land 3 -> playable
* The Legend of Zelda: Link's Awakening -> playable
* Super Mario Land -> playable
* Kirby's Dream Land -> playable
* Pokemon Yellow -> playable with some graphical glitches
* Pokemon Red/Blue -> playable with some graphical glitches
* Little Sound DJ 4.8.6 -> seems to work but useless until audio
  is implemented
* Aretha -> playable

![](https://i.imgur.com/cuw3Z9O.gif)
![](https://i.imgur.com/zj0k5Ch.png)
![](https://i.imgur.com/TiNooIR.png)
![](https://i.imgur.com/TC5ViLl.png)
![](https://i.imgur.com/TO0ZOS6.png)

dependencies: SDL2

should be compatible at least with x86/x86\_64 windows, linux, freebsd
with gcc, clang, msvc

# compiling
just run ```./build``` . it's aware of ```CC```, ```CFLAGS```,
```LDFLAGS``` in case you need to override anything

if you compile with ```./build -DKT_DEBUG``` you will get a full trace
on stdout (SLOW!) which is handy for debugging

# license
this is free and unencumbered software released into the public domain.
refer to the attached UNLICENSE or http://unlicense.org/
