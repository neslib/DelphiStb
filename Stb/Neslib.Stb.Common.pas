unit Neslib.Stb.Common;

interface

const
  {$IF Defined(WIN32)}
  STB_LIB = 'stb32.dll';
  _PU = '';
  {$ELSEIF Defined(WIN64)}
  STB_LIB = 'stb64.dll';
  _PU = '';
  {$ELSEIF Defined(IOS) and Defined(CPUARM32)}
  STB_LIB = 'libstb-ios32.a';
  _PU = '';
  {$ELSEIF Defined(IOS) and Defined(CPUARM64)}
  STB_LIB = 'libstb-ios64.a';
  _PU = '';
  {$ELSEIF Defined(ANDROID)}
  STB_LIB = 'libstb-android.a';
  _PU = '';
  {$ELSEIF Defined(MACOS32) and not Defined(IOS)}
  STB_LIB = 'libstb.dylib';
  _PU = '_';
  {$ELSE}
    {$MESSAGE Error 'Unsupported platform'}
  {$ENDIF}

implementation

end.
