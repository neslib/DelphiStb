unit Neslib.Stb.ImageResize;
{< stb_image_resize: resize images larger/smaller with good quality

  Written with emphasis on usability, portability, and efficiency. (No SIMD or
  threads, so it be easily outperformed by libs that use those.)
  Only scaling and translation is supported, no rotations or shears.
  Easy API downsamples w/Mitchell filter, upsamples w/cubic interpolation.

  @bold(Alpha Channels)

  Most of the resizing functions provide the ability to control how the alpha
  channel of an image is processed. The important things to know about this:

  1. The best mathematically-behaved version of alpha to use is called
     "premultiplied alpha", in which the other color channels have had the alpha
     value multiplied in. If you use premultiplied alpha, linear filtering (such
     as image resampling done by this library, or performed in texture units on
     GPUs) does the "right thing". While premultiplied alpha is standard in the
     movie CGI industry, it is still uncommon in the videogame/real-time world.

     If you linearly filter non-premultiplied alpha, strange effects occur. (For
     example, the 50/50 average of 99% transparent bright green and 1%
     transparent black produces 50% transparent dark green when
     non-premultiplied, whereas premultiplied it produces 50% transparent
     near-black. The former introduces green energy that doesn't exist in the
     source image.)

  2. Artists should not edit premultiplied-alpha images; artists want
     non-premultiplied alpha images. Thus, art tools generally output
     non-premultiplied alpha images.

  3. You will get best results in most cases by converting images to
     premultiplied alpha before processing them mathematically.

  4. If you pass the flag <tt>TStbirAlpha.Premultiplied</tt>, the resizer does
     not do anything special for the alpha channel; it is resampled identically
     to other channels. This produces the correct results for
     premultiplied-alpha images, but produces less-than-ideal results for
     non-premultiplied-alpha images.

  5. If you do not pass the flag <tt>TStbirAlpha.Premultiplied</tt>, then the
     resizer weights the contribution of input pixels based on their alpha
     values, or, equivalently, it multiplies the alpha value into the color
     channels, resamples, then divides by the resultant alpha value. Input
     pixels which have alpha=0 do not contribute at all to output pixels unless
     @bold(all) of the input pixels affecting that output pixel have alpha=0, in
     which case the result for that pixel is the same as it would be without
     <tt>TStbirAlpha.Premultiplied</tt>. However, this is only true for input
     images in integer formats. For input images in float format, input pixels
     with alpha=0 have no effect, and output pixels which have alpha=0 will be 0
     in all channels. (For float images, you can manually achieve the same
     result by adding a tiny epsilon value to the alpha channel of every image,
     and then subtracting or clamping it at the end.)

  6. You can separately control whether the alpha channel is interpreted as
     linear or affected by the colorspace. By default it is linear; you almost
     never want to apply the colorspace. (For example, graphics hardware does
     not apply sRGB conversion to the alpha channel.) }

{$MINENUMSIZE 4}

interface

uses
  Neslib.Stb.Common;

{ Easy-to-use resizing.

  Parameters:
    AInputPixels: points to an array of source image data with ANumChannels
      channels.
    AInputWidth: input image width (x-axis)
    AInputHeight: input image height (y-axis)
    AInputStrideInBytes: the offset between successive rows of image data in
      AInputPixels, in bytes. Tou can specify 0 to mean packed continuously in
      memory.
    AOutputPixels: points to an array of output image data with ANumChannels
      channels.
    AOutputWidth: output image width (x-axis)
    AOutputHeight: output image height (y-axis)
    AOutputStrideInBytes: the offset between successive rows of image data in
      AOutputPixels, in bytes. You can specify 0 to mean packed continuously in
      memory.
    ANumChannels: number of channels in input and output data
      (e.g. RGB=3, RGBA=4).

  Returns:
    True for success, False in case of an error.

  Notes:
  * The input and output pixels can either be in integer (8 bits per channel,
    stbir_resize_uint8) or floating-point (one Single per channel,
    stbir_resize_float) format.
  * Alpha channel is treated identically to other channels.
  * Colorspace is linear or sRGB as specified by function name.
  * Memory required grows approximately linearly with input and output size, but
    with discontinuities at AInputWidth = AOutputWidth and
    AInputHeight = AOutputHeight.
  * These functions use a "default" resampling filter. To change the filter, you
    can use the medium-complexity API. }
function stbir_resize_uint8(
  const AInputPixels: Pointer;
  const AInputWidth, AInputHeight, AInputStrideInBytes: Integer;
  const AOutputPixels: Pointer;
  const AOutputWidth, AOutputHeight, AOutputStrideInBytes: Integer;
  const ANumChannels: Integer): LongBool; cdecl;
  external STB_LIB name _PU + 'stbir_resize_uint8';

function stbir_resize_float(
  const AInputPixels: Pointer;
  const AInputWidth, AInputHeight, AInputStrideInBytes: Integer;
  const AOutputPixels: Pointer;
  const AOutputWidth, AOutputHeight, AOutputStrideInBytes: Integer;
  const ANumChannels: Integer): LongBool; cdecl;
  external STB_LIB name _PU + 'stbir_resize_float';

type
  { Alpha channel behavior when resizing }
  TStbirAlpha = (
    { Use this flag if your texture has premultiplied alpha. Otherwise, stbir
      will use alpha-weighted resampling (effectively premultiplying,
      resampling, then unpremultiplying). }
    Premultiplied = 0,

    { The specified alpha channel should be handled as gamma-corrected value
      even when doing sRGB operations. }
    UsedColorspace = 1);
  TStbirAlphaFlags = set of TStbirAlpha;

type
  { Edge behavior when resizing }
  TStbirEdge = (
    { Replicates colors at the edges }
    Clamp = 1,

    { Reflects colors at the edges }
    Reflect = 2,

    { Wraps colors at the edges }
    Wrap = 3,

    { Sets colors to (transparent) black at the edges }
    Zero = 4);

{ Easy-to-use resizing, but interprets image data as gamma-corrected sRGB.
  Also allows for customized alpha channel and edge clamping behavior.

  Parameters:
    AInputPixels: points to an array of source image data with ANumChannels
      channels.
    AInputWidth: input image width (x-axis)
    AInputHeight: input image height (y-axis)
    AInputStrideInBytes: the offset between successive rows of image data in
      AInputPixels, in bytes. Tou can specify 0 to mean packed continuously in
      memory.
    AOutputPixels: points to an array of output image data with ANumChannels
      channels.
    AOutputWidth: output image width (x-axis)
    AOutputHeight: output image height (y-axis)
    AOutputStrideInBytes: the offset between successive rows of image data in
      AOutputPixels, in bytes. You can specify 0 to mean packed continuously in
      memory.
    ANumChannels: number of channels in input and output data
      (e.g. RGB=3, RGBA=4).
    AAlphaChannel: the index of the alpha channel. Use -1 if you have no alpha
      channel. A value of 0 will probably do the right thing if you're not sure
      what this value means.
    AAlphaFlags: flags to customize alpha channel behavior.
    AEdge: (optional) edge wrap mode

  Returns:
    True for success, False in case of an error.

  Notes:
  * Memory required grows approximately linearly with input and output size, but
    with discontinuities at AInputWidth = AOutputWidth and
    AInputHeight = AOutputHeight.
  * These functions use a "default" resampling filter. To change the filter, you
    can use the medium-complexity API. }
function stbir_resize_uint8_srgb(
  const AInputPixels: Pointer;
  const AInputWidth, AInputHeight, AInputStrideInBytes: Integer;
  const AOutputPixels: Pointer;
  const AOutputWidth, AOutputHeight, AOutputStrideInBytes: Integer;
  const ANumChannels, AAlphaChannel: Integer;
  const AAlphaFlags: TStbirAlphaFlags): LongBool; cdecl;
  external STB_LIB name _PU + 'stbir_resize_uint8_srgb';

function stbir_resize_uint8_srgb_edgemode(
  const AInputPixels: Pointer;
  const AInputWidth, AInputHeight, AInputStrideInBytes: Integer;
  const AOutputPixels: Pointer;
  const AOutputWidth, AOutputHeight, AOutputStrideInBytes: Integer;
  const ANumChannels, AAlphaChannel: Integer;
  const AAlphaFlags: TStbirAlphaFlags; const AEdge: TStbirEdge): LongBool; cdecl;
  external STB_LIB name _PU + 'stbir_resize_uint8_srgb_edgemode';

type
  { Resizing filter }
  TStbirFilter = (
    { Use same filter type that easy-to-use API chooses }
    Default = 0,

    { A trapezoid w/1-pixel wide ramps, same result as box for integer scale
      ratios }
    Box = 1,

    { On upsampling, produces same results as bilinear texture filtering }
    Triangle = 2,

    { The cubic b-spline (aka Mitchell-Netrevalli with B=1,C=0), gaussian-esque }
    CubicBSpline = 3,

    { An interpolating cubic spline }
    CatmullRom = 4,

    { Mitchell-Netrevalli filter with B=1/3, C=1/3 }
    Mitchell = 5);

type
  { Colorspace }
  TStbirColorspace = (
    { Linear colorspace }
    Linear = 0,

    { sRGB colorspace }
    sRGB);

{ Medium-complexity resizing. Adds filter and colorspace selection.

  Parameters:
    AInputPixels: points to an array of source image data with ANumChannels
      channels.
    AInputWidth: input image width (x-axis)
    AInputHeight: input image height (y-axis)
    AInputStrideInBytes: the offset between successive rows of image data in
      AInputPixels, in bytes. Tou can specify 0 to mean packed continuously in
      memory.
    AOutputPixels: points to an array of output image data with ANumChannels
      channels.
    AOutputWidth: output image width (x-axis)
    AOutputHeight: output image height (y-axis)
    AOutputStrideInBytes: the offset between successive rows of image data in
      AOutputPixels, in bytes. You can specify 0 to mean packed continuously in
      memory.
    ANumChannels: number of channels in input and output data
      (e.g. RGB=3, RGBA=4).
    AAlphaChannel: the index of the alpha channel. Use -1 if you have no alpha
      channel. A value of 0 will probably do the right thing if you're not sure
      what this value means.
    AAlphaFlags: flags to customize alpha channel behavior.
    AEdge: edge wrap mode
    AFilter: resize filter to use
    AColorspace: colorspace to use
    AReserved: (optional) pointer that must be nil.

  Returns:
    True for success, False in case of an error.

  The input and output pixels can either be 3 different formats:
  * integer, 8 bits per channel (stbir_resize_uint8_generic)
  * integer, 16-bits per channel (stbir_resize_uint16_generic)
  * floating-point, one Single per channel (stbir_resize_float_generic). }
function stbir_resize_uint8_generic(
  const AInputPixels: Pointer;
  const AInputWidth, AInputHeight, AInputStrideInBytes: Integer;
  const AOutputPixels: Pointer;
  const AOutputWidth, AOutputHeight, AOutputStrideInBytes: Integer;
  const ANumChannels, AAlphaChannel: Integer;
  const AAlphaFlags: TStbirAlphaFlags; const AEdge: TStbirEdge;
  const AFilter: TStbirFilter; const AColorspace: TStbirColorspace;
  const AReserved: Pointer = nil): LongBool; cdecl;
  external STB_LIB name _PU + 'stbir_resize_uint8_generic';

function stbir_resize_uint16_generic(
  const AInputPixels: Pointer;
  const AInputWidth, AInputHeight, AInputStrideInBytes: Integer;
  const AOutputPixels: Pointer;
  const AOutputWidth, AOutputHeight, AOutputStrideInBytes: Integer;
  const ANumChannels, AAlphaChannel: Integer;
  const AAlphaFlags: TStbirAlphaFlags; const AEdge: TStbirEdge;
  const AFilter: TStbirFilter; const AColorspace: TStbirColorspace;
  const AReserved: Pointer = nil): LongBool; cdecl;
  external STB_LIB name _PU + 'stbir_resize_uint16_generic';

function stbir_resize_float_generic(
  const AInputPixels: Pointer;
  const AInputWidth, AInputHeight, AInputStrideInBytes: Integer;
  const AOutputPixels: Pointer;
  const AOutputWidth, AOutputHeight, AOutputStrideInBytes: Integer;
  const ANumChannels, AAlphaChannel: Integer;
  const AAlphaFlags: TStbirAlphaFlags; const AEdge: TStbirEdge;
  const AFilter: TStbirFilter; const AColorspace: TStbirColorspace;
  const AReserved: Pointer = nil): LongBool; cdecl;
  external STB_LIB name _PU + 'stbir_resize_float_generic';

type
  { Data type for pixels }
  TStbirDatatype = (
    { Unsigned 8-bits }
    UInt8,

    { Unsigned 16-bits }
    UInt16,

    { Unsigned 32-bits }
    UInt32,

    { Single precision floating-point }
    Float);

{ Full-complexity resizing. Adds custom pixel data type and separate filter and
  edge modes for each axis.

  Parameters:
    AInputPixels: points to an array of source image data with ANumChannels
      channels.
    AInputWidth: input image width (x-axis)
    AInputHeight: input image height (y-axis)
    AInputStrideInBytes: the offset between successive rows of image data in
      AInputPixels, in bytes. Tou can specify 0 to mean packed continuously in
      memory.
    AOutputPixels: points to an array of output image data with ANumChannels
      channels.
    AOutputWidth: output image width (x-axis)
    AOutputHeight: output image height (y-axis)
    AOutputStrideInBytes: the offset between successive rows of image data in
      AOutputPixels, in bytes. You can specify 0 to mean packed continuously in
      memory.
    ADataType: the data type of the pixels.
    ANumChannels: number of channels in input and output data
      (e.g. RGB=3, RGBA=4).
    AAlphaChannel: the index of the alpha channel. Use -1 if you have no alpha
      channel. A value of 0 will probably do the right thing if you're not sure
      what this value means.
    AAlphaFlags: flags to customize alpha channel behavior.
    AEdgeHorizontal: edge wrap mode in horizontal direction
    AEdgeVertical: edge wrap mode in vertical direction
    AFilterHorizontal: resize filter in horizontal direction
    AFilterVertical: resize filter in vertical direction
    AColorspace: colorspace to use
    AReserved: (optional) pointer that must be nil.

  Returns:
    True for success, False in case of an error. }
function stbir_resize(
  const AInputPixels: Pointer;
  const AInputWidth, AInputHeight, AInputStrideInBytes: Integer;
  const AOutputPixels: Pointer;
  const AOutputWidth, AOutputHeight, AOutputStrideInBytes: Integer;
  const ADataType: TStbirDatatype; const ANumChannels, AAlphaChannel: Integer;
  const AAlphaFlags: TStbirAlphaFlags;
  const AEdgeHorizontal, AEdgeVertical: TStbirEdge;
  const AFilterHorizontal, AFilterVertical: TStbirFilter;
  const AColorspace: TStbirColorspace;
  const AReserved: Pointer = nil): LongBool; cdecl;
  external STB_LIB name _PU + 'stbir_resize';

{ Full-complexity resizing. Adds subpixel customization.

  Parameters:
    AInputPixels: points to an array of source image data with ANumChannels
      channels.
    AInputWidth: input image width (x-axis)
    AInputHeight: input image height (y-axis)
    AInputStrideInBytes: the offset between successive rows of image data in
      AInputPixels, in bytes. Tou can specify 0 to mean packed continuously in
      memory.
    AOutputPixels: points to an array of output image data with ANumChannels
      channels.
    AOutputWidth: output image width (x-axis)
    AOutputHeight: output image height (y-axis)
    AOutputStrideInBytes: the offset between successive rows of image data in
      AOutputPixels, in bytes. You can specify 0 to mean packed continuously in
      memory.
    ADataType: the data type of the pixels.
    ANumChannels: number of channels in input and output data
      (e.g. RGB=3, RGBA=4).
    AAlphaChannel: the index of the alpha channel. Use -1 if you have no alpha
      channel. A value of 0 will probably do the right thing if you're not sure
      what this value means.
    AAlphaFlags: flags to customize alpha channel behavior.
    AEdgeHorizontal: edge wrap mode in horizontal direction
    AEdgeVertical: edge wrap mode in vertical direction
    AFilterHorizontal: resize filter in horizontal direction
    AFilterVertical: resize filter in vertical direction
    AColorspace: colorspace to use
    AReserved: pointer that must be nil.
    AXScale: subpixel scale in horizontal direction
    AYScale: subpixel scale in vertical direction
    AXOffset: subpixel offset in horizontal direction
    AYOffset: subpixel offset in vertical direction

  Returns:
    True for success, False in case of an error. }
function stbir_resize_subpixel(
  const AInputPixels: Pointer;
  const AInputWidth, AInputHeight, AInputStrideInBytes: Integer;
  const AOutputPixels: Pointer;
  const AOutputWidth, AOutputHeight, AOutputStrideInBytes: Integer;
  const ADataType: TStbirDatatype; const ANumChannels, AAlphaChannel: Integer;
  const AAlphaFlags: TStbirAlphaFlags;
  const AEdgeHorizontal, AEdgeVertical: TStbirEdge;
  const AFilterHorizontal, AFilterVertical: TStbirFilter;
  const AColorspace: TStbirColorspace; const AReserved: Pointer;
  const AXScale, AYScale, AXOffset, AYOffset: Single): LongBool; cdecl;
  external STB_LIB name _PU + 'stbir_resize_subpixel';

{ Full-complexity resizing. Specify the source region to use using texture
  coordinates.

  Parameters:
    AInputPixels: points to an array of source image data with ANumChannels
      channels.
    AInputWidth: input image width (x-axis)
    AInputHeight: input image height (y-axis)
    AInputStrideInBytes: the offset between successive rows of image data in
      AInputPixels, in bytes. Tou can specify 0 to mean packed continuously in
      memory.
    AOutputPixels: points to an array of output image data with ANumChannels
      channels.
    AOutputWidth: output image width (x-axis)
    AOutputHeight: output image height (y-axis)
    AOutputStrideInBytes: the offset between successive rows of image data in
      AOutputPixels, in bytes. You can specify 0 to mean packed continuously in
      memory.
    ADataType: the data type of the pixels.
    ANumChannels: number of channels in input and output data
      (e.g. RGB=3, RGBA=4).
    AAlphaChannel: the index of the alpha channel. Use -1 if you have no alpha
      channel. A value of 0 will probably do the right thing if you're not sure
      what this value means.
    AAlphaFlags: flags to customize alpha channel behavior.
    AEdgeHorizontal: edge wrap mode in horizontal direction
    AEdgeVertical: edge wrap mode in vertical direction
    AFilterHorizontal: resize filter in horizontal direction
    AFilterVertical: resize filter in vertical direction
    AColorspace: colorspace to use
    AReserved: pointer that must be nil.
    AS0, AT0: top-left corner of the source region to use. Specified as texture
      cordinates (where 0 equals the top or left, and 1 equals right or bottom).
    AS1, AT1: bottom-right corner of the source region to use.

  Returns:
    True for success, False in case of an error. }
function stbir_resize_region(
  const AInputPixels: Pointer;
  const AInputWidth, AInputHeight, AInputStrideInBytes: Integer;
  const AOutputPixels: Pointer;
  const AOutputWidth, AOutputHeight, AOutputStrideInBytes: Integer;
  const ADataType: TStbirDatatype; const ANumChannels, AAlphaChannel: Integer;
  const AAlphaFlags: TStbirAlphaFlags;
  const AEdgeHorizontal, AEdgeVertical: TStbirEdge;
  const AFilterHorizontal, AFilterVertical: TStbirFilter;
  const AColorspace: TStbirColorspace; const AReserved: Pointer;
  const AS0, AT0, AS1, AT1: Single): LongBool; cdecl;
  external STB_LIB name _PU + 'stbir_resize_region';

implementation

end.
