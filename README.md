game boy emulator in c89
at the moment it only runs tetris and the dmg bootrom and has no sound,
but I'm gonna start working towards making it run pokemon

![](https://i.imgur.com/cuw3Z9O.gif)
![](https://i.imgur.com/TC5ViLl.png)
![](https://i.imgur.com/4Ou7fGI.png)

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
