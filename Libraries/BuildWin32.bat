cl /GL /analyze- /W3 /Gy /Zc:wchar_t /I"..\C" /Gm- /O2 /sdl- /Zc:inline /fp:precise /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /fp:except- /errorReport:prompt /WX- /Zc:forScope /arch:SSE2 /Gd /Oy- /Oi /MT /LD /Fe"Windows32/stb.dll" /DEF stb.def stb.c
del stb.obj
cd Windows32
del stb32.dll
ren stb.dll stb32.dll
del stb.*
cd ..