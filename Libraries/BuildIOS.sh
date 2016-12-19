IOS_SDK=

OUTPUT="../Stb/libstb"
PLATFORM="iPhoneOS"

DEVELOPER_DIR=`xcode-select -print-path`
if [ ! -d $DEVELOPER_DIR ]; then
  echo "Please set up Xcode correctly. '$DEVELOPER_DIR' is not a valid developer tools folder."
  exit 1
fi

SDK_ROOT=$DEVELOPER_DIR/Platforms/$PLATFORM.platform/Developer/SDKs/$PLATFORM$IOS_SDK.sdk
if [ ! -d $SDK_ROOT ]; then
  echo "The iOS SDK $IOS_SDK was not found in $SDK_ROOT."
  exit 1
fi

rm *.o
rm $OUTPUT-ios32.a
rm $OUTPUT-ios64.a

clang -c -O3 -arch armv7 -isysroot $SDK_ROOT -I"../C" -DSTBI_NEON stb.c
ar rcs $OUTPUT-ios32.a stb.o
ranlib $OUTPUT-ios32.a

clang -c -O3 -arch arm64 -isysroot $SDK_ROOT -I"../C" -DSTBI_NEON stb.c
ar rcs $OUTPUT-ios64.a stb.o
ranlib $OUTPUT-ios64.a

rm *.o