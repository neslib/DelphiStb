cl /GL /analyze- /W3 /Gy /Zc:wchar_t /I"..\C" /Gm- /O2 /sdl- /Zc:inline /fp:precise /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /fp:except- /errorReport:prompt /WX- /Zc:forScope /Gd /Oy- /Oi /MT /LD /Fe"Windows64/stb.dll" /DEF stb.def stb.c
del stb.obj
cd Windows64
del stb64.dll
ren stb.dll stb64.dll
del stb.*
cd ..