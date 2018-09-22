unit Neslib.Stb.Image;
{< stb_image: image loading/decoding from file/memory: JPG, PNG, TGA, BMP, PSD,
  GIF, HDR, PIC

  Quick notes:
  * Primarily of interest to game developers and other people who can
    avoid problematic images and only need the trivial interface
  * JPEG baseline & progressive (12 bpc/arithmetic not supported, same as stock
    IJG lib)
  * PNG 1/2/4/8/16-bit-per-channel
  * TGA (not sure what subset, if a subset)
  * BMP non-1bpp, non-RLE
  * PSD (composited view only, no extra channels, 8/16 bit-per-channel)
  * GIF (always reported as 4-channel)
  * HDR (radiance rgbE format)
  * PIC (Softimage PIC)
  * PNM (PPM and PGM binary only)
  * decode from memory or file
  * decode from arbitrary I/O callbacks
  * SIMD acceleration on x86/x64 (SSE2) and ARM (NEON)

  Limitations:
  * no 12-bit-per-channel JPEG
  * no JPEGs with arithmetic coding
  * GIF always returns 4 components per pixel

  The return value from an image loader is a pointer which points to the pixel
  data, or nil on an allocation failure or if the image is corrupt or invalid.

  The pixel data consists of <tt>AHeight</tt> scanlines of <tt>AWidth</tt>
  pixels, with each pixel consisting of <tt>AComponents</tt> interleaved 8-bit
  components; the first pixel pointed to is top-left-most in the image. There is
  no padding between image scanlines or between pixels, regardless of format.

  The number of components <tt>AComponents</tt> is <tt>ADesiredChannels</tt>
  if <tt>ADesiredChannels</tt> is non-zero, or <tt>AChannelsInFile</tt>
  otherwise. If <tt>ADesiredChannels</tt> is non-zero, <tt>AChannelsInFile</tt>
  has the number of components that @bold(would) have been output otherwise.
  E.g. if you set <tt>ADesiredChannels</tt> to 4, you will always get RGBA
  output, but you can check <tt>AChannelsInFile</tt> to see if it's trivially
  opaque because e.g. there were only 3 channels in the source image.

  An output image with N components has the following components interleaved
  in this order in each pixel:
  * N=1: grey
  * N=2: grey, alpha
  * N=3: red, green, blue
  * N=4: red, green, blue, alpha

  If image loading fails for any reason, the return value will be nil, and
  <tt>AWidth</tt>, <tt>AHeight</tt>, <tt>AChannelsInFile</tt> will be unchanged.
  The function <tt>stbi_failure_reason</tt> can be queried for an extremely
  brief, end-user unfriendly explanation of why the load failed.

  Paletted PNG, BMP, GIF, and PIC images are automatically depalettized.

  @bold(I/O callbacks)

  I/O callbacks allow you to read from arbitrary sources, like packaged files or
  some other source. Data read from callbacks are processed through a small
  internal buffer (currently 128 bytes) to try to reduce overhead.

  The three functions you must define are "read" (reads some bytes of data),
  "skip" (skips some bytes of data), "eof" (reports if the stream is at the
  end).

  @bold(SIMD support)

  The JPEG decoder will try to automatically use SIMD kernels on x86 and Neon
  on ARM.

  On x86, SSE2 will automatically be used when available based on a run-time
  test; if not, the generic C versions are used as a fall-back.

  @bold(HDR image support)

  stb_image now supports loading HDR images in general, and currently the
  Radiance .HDR file format, although the support is provided generically. You
  can still load any file through the existing interface; if you attempt to load
  an HDR file, it will be automatically remapped to LDR, assuming gamma 2.2 and
  an arbitrary scale factor defaulting to 1; both of these constants can be
  reconfigured through this interface:

  <source>
  stbi_hdr_to_ldr_gamma(2.2);
  stbi_hdr_to_ldr_scale(1.0);
  </source>

  Additionally, there is a new, parallel interface (<tt>stb_loadf*</tt) for
  loading files as (linear) floats to preserve the full dynamic range.

  If you load LDR images through this interface, those images will be promoted
  to floating point values, run through the inverse of constants corresponding
  to the above:

  <source>
  stbi_ldr_to_hdr_scale(1.0);
  stbi_ldr_to_hdr_gamma(2.2);
  </source>

  Finally, given a filename (or an open file or memory block) containing image
  data, you can query for the "most appropriate" interface to use (that is,
  whether the image is HDR or not), using <tt>stbi_is_hdr</tt>.

  @bold(iPhone PNG support)

  By default we convert iphone-formatted PNGs back to RGB, even though they are
  internally encoded differently. You can disable this conversion by by calling
  <tt>stbi_convert_iphone_png_to_rgb(False)</tt>, in which case you will always
  just get the native iphone "format" through (which is BGR stored in RGB).

  Call <tt>stbi_set_unpremultiply_on_load(True)</tt> as well to force a divide
  per pixel to remove any premultiplied alpha *only* if the image file
  explicitly says there's premultiplied data (currently only happens in iPhone
  images, and only if iPhone convert-to-rgb processing is on). }

interface

uses
  Neslib.Stb.Common;

type
  { Callback structure for loading images using callbacks (eg.
    stbi_load_from_callbacks) }
  TStbiIOCallbacks = record
    { Is called to read data.

      Parameters:
        AUserData: user data passed to the load function.
        AData: pointer to the buffer to read into.
        ASize: number of bytes to read.

      Returns:
        You must return the number of bytes actually read. }
    Read: function (const AUserData, AData: Pointer; const ASize: Integer): Integer; cdecl;

    { Is called to skip data.

      Parameters:
        AUserData: user data passed to the load function.
        ASize: number of bytes to skip. }
    Skip: procedure (const AUserData: Pointer; const ASize: Integer); cdecl;

    { Is called to check if the end of the data has been reached.

      Parameters:
        AUserData: user data passed to the load function.

      Returns:
        You must return True if the end has been reached, or False otherwise. }
    Eof: function (const AUserData: Pointer): LongBool; cdecl;
  end;
  PStbiIOCallbacks = ^TStbiIOCallbacks;

{ Loads an image from a file.

  Parameters:
    AFilename: name of the file containing the image.
    AWidth: is set to the width of the image.
    AHeight: is set to the height of the image.
    AChannelsInFile: is set to the number of components in the image.
    ADesiredChannels: number of components you want to return.

  Returns:
    A pointer to the image data. You must free this later with stbi_image_free. }
function stbi_load(const AFilename: MarshaledAString; out AWidth, AHeight,
  AChannelsInFile: Integer; const ADesiredChannels: Integer = 0): Pointer; cdecl;
  external STB_LIB name _PU + 'stbi_load';

{ Loads an image from memory.

  Parameters:
    ABuffer: pointer to the source image data.
    ALen: number of bytes in ABuffer.
    AWidth: is set to the width of the image.
    AHeight: is set to the height of the image.
    AChannelsInFile: is set to the number of components in the image.
    ADesiredChannels: number of components you want to return.

  Returns:
    A pointer to the image data. You must free this later with stbi_image_free. }
function stbi_load_from_memory(const ABuffer: Pointer; const ALen: Integer;
  out AWidth, AHeight, AChannelsInFile: Integer;
  const ADesiredChannels: Integer = 0): Pointer; cdecl;
  external STB_LIB name _PU + 'stbi_load_from_memory';

{ Loads an image from callbacks.

  Parameters:
    ACallbacks: the callback that will be called to read the source data.
    AUserData: user-defined data that will be passed to the callbacks.
    AWidth: is set to the width of the image.
    AHeight: is set to the height of the image.
    AChannelsInFile: is set to the number of components in the image.
    ADesiredChannels: number of components you want to return.

  Returns:
    A pointer to the image data. You must free this later with stbi_image_free. }
function stbi_load_from_callbacks(var ACallbacks: TStbiIOCallbacks;
  const AUserData: Pointer; out AWidth, AHeight, AChannelsInFile: Integer;
  const ADesiredChannels: Integer = 0): Pointer; cdecl;
  external STB_LIB name _PU + 'stbi_load_from_callbacks';

{ Loads an image from a file. The image data is returned as (linear) floating-
  point values, which is useful to preserve the full dynamic range of HDR
  images.

  Parameters:
    AFilename: name of the file containing the image.
    AWidth: is set to the width of the image.
    AHeight: is set to the height of the image.
    AChannelsInFile: is set to the number of components in the image.
    ADesiredChannels: number of components you want to return.

  Returns:
    A pointer to the image data. You must free this later with stbi_image_free. }
function stbi_loadf(const AFilename: MarshaledAString; out AWidth, AHeight,
  AChannelsInFile: Integer; const ADesiredChannels: Integer = 0): Pointer; cdecl;
  external STB_LIB name _PU + 'stbi_loadf';

{ Loads an image from memory. The image data is returned as (linear) floating-
  point values, which is useful to preserve the full dynamic range of HDR
  images.

  Parameters:
    ABuffer: pointer to the source image data.
    ALen: number of bytes in ABuffer.
    AWidth: is set to the width of the image.
    AHeight: is set to the height of the image.
    AChannelsInFile: is set to the number of components in the image.
    ADesiredChannels: number of components you want to return.

  Returns:
    A pointer to the image data. You must free this later with stbi_image_free. }
function stbi_loadf_from_memory(const ABuffer: Pointer; const ALen: Integer;
  out AWidth, AHeight, AChannelsInFile: Integer;
  const ADesiredChannels: Integer = 0): Pointer; cdecl;
  external STB_LIB name _PU + 'stbi_loadf_from_memory';

{ Loads an image from callbacks. The image data is returned as (linear)
  floating-point values, which is useful to preserve the full dynamic range of
  HDR images.

  Parameters:
    ACallbacks: the callback that will be called to read the source data.
    AUserData: user-defined data that will be passed to the callbacks.
    AWidth: is set to the width of the image.
    AHeight: is set to the height of the image.
    AChannelsInFile: is set to the number of components in the image.
    ADesiredChannels: number of components you want to return.

  Returns:
    A pointer to the image data. You must free this later with stbi_image_free. }
function stbi_loadf_from_callbacks(var ACallbacks: TStbiIOCallbacks;
  const AUserData: Pointer; out AWidth, AHeight, AChannelsInFile: Integer;
  const ADesiredChannels: Integer = 0): Pointer; cdecl;
  external STB_LIB name _PU + 'stbi_loadf_from_callbacks';

{ HDR files will be automatically remapped to LDR files using a gamma of 2.2.
  Use this routine to change this gamma value.

  Parameters:
    AGamma: the gamma value to use to remap HDR to LDR.

  You must call this routine before loading a HDR image. }
procedure stbi_hdr_to_ldr_gamma(const AGamma: Single); cdecl;
  external STB_LIB name _PU + 'stbi_hdr_to_ldr_gamma';

{ HDR files will be automatically remapped to LDR files using a scale factor of
  1.0. Use this routine to change this scale factor value.

  Parameters:
    AScale: the scale factor to use to remap HDR to LDR.

  You must call this routine before loading a HDR image. }
procedure stbi_hdr_to_ldr_scale(const AScale: Single); cdecl;
  external STB_LIB name _PU + 'stbi_hdr_to_ldr_scale';

{ You can use one of the <tt>stbi_loadf*</tt> functions to load an image as
  floating-point data. However, if that image is in LDR format, they will be
  promoted to HDR using a gamma of 2.2.
  Use this routine to change this gamma value.

  Parameters:
    AGamma: the gamma value to use to promote LDR to HDR.

  You must call this routine before calling a <tt>stbi_loadf*</tt> funciton. }
procedure stbi_ldr_to_hdr_gamma(const AGamma: Single); cdecl;
  external STB_LIB name _PU + 'stbi_ldr_to_hdr_gamma';

{ You can use one of the <tt>stbi_loadf*</tt> functions to load an image as
  floating-point data. However, if that image is in LDR format, they will be
  promoted to HDR using a scale factor of 1.0.
  Use this routine to change this scale factor.

  Parameters:
    AScale: the scale factor to use to to promote LDR to HDR.

  You must call this routine before calling a <tt>stbi_loadf*</tt> funciton. }
procedure stbi_ldr_to_hdr_scale(const AScale: Single); cdecl;
  external STB_LIB name _PU + 'stbi_ldr_to_hdr_scale';

{ Checks if an image file is in HDR format.

  Parameters:
    AFilename: name of the file containing the image.

  Returns:
    True if the source data is HDR. }
function stbi_is_hdr(const AFilename: MarshaledAString): LongBool; cdecl;
  external STB_LIB name _PU + 'stbi_is_hdr';

{ Checks if an image buffer is in HDR format.

  Parameters:
    ABuffer: pointer to the source image data.
    ALen: number of bytes in ABuffer.

  Returns:
    True if the source data is HDR. }
function stbi_is_hdr_from_memory(var ACallbacks: TStbiIOCallbacks;
  const AUserData: Pointer): LongBool; cdecl;
  external STB_LIB name _PU + 'stbi_is_hdr_from_memory';

{ Checks if an image buffer is in HDR format.
  Uses callbacks to read the source data.

  Parameters:
    ACallbacks: the callback that will be called to read the source data.
    AUserData: user-defined data that will be passed to the callbacks.

  Returns:
    True if the source data is HDR. }
function stbi_is_hdr_from_callbacks(var ACallbacks: TStbiIOCallbacks;
  const AUserData: Pointer): LongBool; cdecl;
  external STB_LIB name _PU + 'stbi_is_hdr_from_callbacks';

{ Return a failure reason for when loading image has failed.

  Returns:
    An extremely brief, end-user unfriendly explanation of why the load failed.

  This routine is @bold(not) thread-safe }
function stbi_failure_reason: MarshaledAString; cdecl;
  external STB_LIB name _PU + 'stbi_failure_reason';

{ Frees a previously loaded image. You must eventually call this routine for
  every imaged loaded using one of the <tt>stbi_load*</tt> functions.

  Parameters:
    AData: the image data returned from one of the <tt>stbi_load*</tt>
      functions. }
procedure stbi_image_free(const AData: Pointer); cdecl;
  external STB_LIB name _PU + 'stbi_image_free';

{ Returns image metadata from a file without fully decoding the image.

  Parameters:
    AFilename: name of the file containing the image.
    AWidth: is set to the width of the image.
    AHeight: is set to the height of the image.
    AComponents: is set to the number of components in the image.

  Returns:
    True if the image is valid or False otherwise. }
function stbi_info(const AFilename: MarshaledAString; out AWidth, AHeight,
  AComponents: Integer): LongBool; cdecl;
  external STB_LIB name _PU + 'stbi_info';

{ Returns image metadata from a memory buffer without fully decoding the image.

  Parameters:
    ABuffer: pointer to the source image data.
    ALen: number of bytes in ABuffer.
    AWidth: is set to the width of the image.
    AHeight: is set to the height of the image.
    AComponents: is set to the number of components in the image.

  Returns:
    A pointer to the image data. You must free this later with stbi_image_free. }
function stbi_info_from_memory(const ABuffer: Pointer; const ALen: Integer;
  out AWidth, AHeight, AComponents: Integer): LongBool; cdecl;
  external STB_LIB name _PU + 'stbi_info_from_memory';

{ Returns image metadata without fully decoding the image.
  Uses callbacks to read the source data.

  Parameters:
    ACallbacks: the callback that will be called to read the source data.
    AUserData: user-defined data that will be passed to the callbacks.
    AWidth: is set to the width of the image.
    AHeight: is set to the height of the image.
    AComponents: is set to the number of components in the image.

  Returns:
    A pointer to the image data. You must free this later with stbi_image_free. }
function stbi_info_from_callbacks(var ACallbacks: TStbiIOCallbacks;
  const AUserData: Pointer; out AWidth, AHeight, AComponents: Integer): LongBool; cdecl;
  external STB_LIB name _PU + 'stbi_info_from_callbacks';

{ For image formats that explicitly notate that they have premultiplied alpha,
  we just return the colors as stored in the file. Set this flag to force
  unpremultiplication.

  Parameters:
    AUnpremultiply: whether to reverse alpha premultiplication when loading
      premultiplied files.

  Results are undefined if the unpremultiply overflow.}
procedure stbi_set_unpremultiply_on_load(const AUnpremultiply: LongBool); cdecl;
  external STB_LIB name _PU + 'stbi_set_unpremultiply_on_load';

{ Indicate whether we should process iphone images back to canonical format,
  or just pass them through "as-is".

  Parameters:
    AConvert: whether to convert to canonical RGB. }
procedure stbi_convert_iphone_png_to_rgb(const AConvert: LongBool); cdecl;
  external STB_LIB name _PU + 'stbi_convert_iphone_png_to_rgb';

{ Flip the image vertically on load, so the first pixel in the output array is
  the bottom left.

  Parameters:
    AFlip: whether to flip when loading images. }
procedure stbi_set_flip_vertically_on_load(const AFlip: LongBool); cdecl;
  external STB_LIB name _PU + 'stbi_set_flip_vertically_on_load';

implementation

end.
