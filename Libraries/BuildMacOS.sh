OUTPUT="MacOS/libstb.dylib"
PLATFORM="MacOSX"
MIN_VERSION="10.10"

DEVELOPER_DIR=`xcode-select -print-path`
if [ ! -d $DEVELOPER_DIR ]; then
  echo "Please set up Xcode correctly. '$DEVELOPER_DIR' is not a valid developer tools folder."
  exit 1
fi

SDK_ROOT=$DEVELOPER_DIR/Platforms/$PLATFORM.platform/Developer/SDKs/$PLATFORM.sdk
if [ ! -d $SDK_ROOT ]; then
  echo "The MacOS SDK was not found in $SDK_ROOT."
  exit 1
fi

clang -dynamiclib -m32 -O3 -isysroot $SDK_ROOT -I"../C" -mmacosx-version-min=$MIN_VERSION -o $OUTPUT stb.c