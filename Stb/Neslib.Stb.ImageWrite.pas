unit Neslib.Stb.ImageWrite;
{< stb_image_write: image writing to disk: PNG, TGA, BMP.

  The PNG output is not optimal; it is 20-50% larger than the file written by a
  decent optimizing implementation; though providing a custom zlib compress
  function (see STBIW_ZLIB_COMPRESS) can mitigate that. This library is designed
  for source code compactness and simplicity, not optimal image file size or
  run-time performance.

  The functions in this unit create an image file defined by the parameters. The
  image is a rectangle of pixels stored from left-to-right, top-to-bottom.
  Each pixel contains <tt>AComponents</tt> channels of data stored interleaved
  with 8-bits per channel, in the following order:
  * 1=Gray
  * 2=Gray, Alpha
  * 3=Red, Green, Blue
  * 4=Red, Green, Blue, Alpha

  The rectangle is <tt>AWidth</tt> pixels wide and <tt>AHeight</tt> pixels tall.
  The <tt>AData</tt> pointer points to the first byte of the top-left-most
  pixel.

  For PNG, <tt>AStrideInBytes</tt> is the distance in bytes from the first byte
  of a row of pixels to the first byte of the next row of pixels.

  PNG creates output files with the same number of components as the input.
  The BMP format expands Gray to RGB in the file format and does not output
  alpha.

  PNG supports writing rectangles of data even when the bytes storing rows of
  data are not consecutive in memory (e.g. sub-rectangles of a larger image),
  by supplying the stride between the beginning of adjacent rows. The other
  formats do not. (Thus you cannot write a native-format BMP through the BMP
  writer, both because it is in BGR order and because it may have padding
  at the end of the line.)

  PNG allows you to set the deflate compression level by setting the global
  variable 'stbi_write_png_compression_level' (it defaults to 8).

  HDR expects linear float data. Since the format is always 32-bit rgb(e) data,
  alpha (if provided) is discarded, and for monochrome data it is replicated
  across all three channels.

  TGA supports RLE compressed data.

  JPEG does ignore alpha channels in input data; quality is between 1 and 100.
  Higher quality looks better but results in a bigger image.
  JPEG baseline (no JPEG progressive). }

interface

uses
  Neslib.Stb.Common;

{ Writes an image in PNG format.

  Parameters:
    AFilename: name of the file to create.
    AWidth: width of the image.
    AHeight: height of the image.
    AComponents: number of components in the image.
    AData: pointer to the image data.
    AStrideInBytes: the distance in bytes from the first byte of a row of pixels
      to the first byte of the next row of pixels.

  Returns:
    True on failure, False on success.

  PNG creates output files with the same number of components as the input.
  PNG supports writing rectangles of data even when the bytes storing rows of
  data are not consecutive in memory (e.g. sub-rectangles of a larger image),
  by supplying the stride between the beginning of adjacent rows. }
function stbi_write_png(const AFilename: MarshaledAString; const AWidth,
  AHeight, AComponents: Integer; const AData: Pointer;
  const AStrideInBytes: Integer): LongBool; cdecl;
  external STB_LIB name _PU + 'stbi_write_png';

{ Writes an image in BMP format.

  Parameters:
    AFilename: name of the file to create.
    AWidth: width of the image.
    AHeight: height of the image.
    AComponents: number of components in the image.
    AData: pointer to the image data.

  Returns:
    True on failure, False on success.

  The BMP format expands Gray to RGB in the file format and does not output
  alpha. }
function stbi_write_bmp(const AFilename: MarshaledAString; const AWidth,
  AHeight, AComponents: Integer; const AData: Pointer): LongBool; cdecl;
  external STB_LIB name _PU + 'stbi_write_bmp';

{ Writes an image in TGA format.

  Parameters:
    AFilename: name of the file to create.
    AWidth: width of the image.
    AHeight: height of the image.
    AComponents: number of components in the image.
    AData: pointer to the image data.

  Returns:
    True on failure, False on success.

  TGA supports RLE compressed data. }
function stbi_write_tga(const AFilename: MarshaledAString; const AWidth,
  AHeight, AComponents: Integer; const AData: Pointer): LongBool; cdecl;
  external STB_LIB name _PU + 'stbi_write_tga';

{ Writes an image in HDR format.

  Parameters:
    AFilename: name of the file to create.
    AWidth: width of the image.
    AHeight: height of the image.
    AComponents: number of components in the image.
    AData: pointer to the image data. Must be floating-point data.

  Returns:
    True on failure, False on success.

  HDR expects linear float data. Since the format is always 32-bit rgb(e) data,
  alpha (if provided) is discarded, and for monochrome data it is replicated
  across all three channels. }
function stbi_write_hdr(const AFilename: MarshaledAString; const AWidth,
  AHeight, AComponents: Integer; const AData: Pointer): LongBool; cdecl;
  external STB_LIB name _PU + 'stbi_write_hdr';

{ Writes an image in Jpeg format.

  Parameters:
    AFilename: name of the file to create.
    AWidth: width of the image.
    AHeight: height of the image.
    AComponents: number of components in the image.
    AData: pointer to the image data. Must be floating-point data.
    AQuality: the quality level (1-100).

  Returns:
    True on failure, False on success.

  HDR expects linear float data. Since the format is always 32-bit rgb(e) data,
  alpha (if provided) is discarded, and for monochrome data it is replicated
  across all three channels. }
function stbi_write_jpg(const AFilename: MarshaledAString; const AWidth,
  AHeight, AComponents: Integer; const AData: Pointer; const AQuality: Integer): LongBool; cdecl;
  external STB_LIB name _PU + 'stbi_write_jpg';

type
  { Write function type for the <tt>stbi_write_*_to_func</tt> functions.

    Parameters:
      AUserData: the user data passed to the <tt>stbi_write_*_to_func</tt>
        funciton.
      AData: pointer to the data to write.
      ASize: size of the data to write. }
  TStbiWriteFunc = procedure(const AUserData, AData: Pointer;
    const ASize: Integer); cdecl;

{ Writes an image in PNG format using a write function.

  Parameters:
    AWriteFunc: to function that is called to write the data
    AUserData: user-defined data that will be passed to the write function.
    AWidth: width of the image.
    AHeight: height of the image.
    AComponents: number of components in the image.
    AData: pointer to the image data.
    AStrideInBytes: the distance in bytes from the first byte of a row of pixels
      to the first byte of the next row of pixels.

  Returns:
    True on failure, False on success.

  PNG creates output files with the same number of components as the input.
  PNG supports writing rectangles of data even when the bytes storing rows of
  data are not consecutive in memory (e.g. sub-rectangles of a larger image),
  by supplying the stride between the beginning of adjacent rows. }
function stbi_write_png_to_func(const AWriteFunc: TStbiWriteFunc;
  const AUserData: Pointer; const AWidth, AHeight, AComponents: Integer;
  const AData: Pointer; const AStrideInBytes: Integer): LongBool; cdecl;
  external STB_LIB name _PU + 'stbi_write_png_to_func';

{ Writes an image in BMP format using a write function.

  Parameters:
    AWriteFunc: to function that is called to write the data
    AUserData: user-defined data that will be passed to the write function.
    AWidth: width of the image.
    AHeight: height of the image.
    AComponents: number of components in the image.
    AData: pointer to the image data.

  Returns:
    True on failure, False on success.

  The BMP format expands Gray to RGB in the file format and does not output
  alpha. }
function stbi_write_bmp_to_func(const AWriteFunc: TStbiWriteFunc;
  const AUserData: Pointer; const AWidth, AHeight, AComponents: Integer;
  const AData: Pointer): LongBool; cdecl;
  external STB_LIB name _PU + 'stbi_write_bmp_to_func';

{ Writes an image in TGA format using a write function.

  Parameters:
    AWriteFunc: to function that is called to write the data
    AUserData: user-defined data that will be passed to the write function.
    AWidth: width of the image.
    AHeight: height of the image.
    AComponents: number of components in the image.
    AData: pointer to the image data.

  Returns:
    True on failure, False on success.

  TGA supports RLE compressed data. }
function stbi_write_tga_to_func(const AWriteFunc: TStbiWriteFunc;
  const AUserData: Pointer; const AWidth, AHeight, AComponents: Integer;
  const AData: Pointer): LongBool; cdecl;
  external STB_LIB name _PU + 'stbi_write_tga_to_func';

{ Writes an image in HDR format using a write function.

  Parameters:
    AWriteFunc: to function that is called to write the data
    AUserData: user-defined data that will be passed to the write function.
    AWidth: width of the image.
    AHeight: height of the image.
    AComponents: number of components in the image.
    AData: pointer to the image data. Must be floating-point data.

  Returns:
    True on failure, False on success.

  HDR expects linear float data. Since the format is always 32-bit rgb(e) data,
  alpha (if provided) is discarded, and for monochrome data it is replicated
  across all three channels. }
function stbi_write_hdr_to_func(const AWriteFunc: TStbiWriteFunc;
  const AUserData: Pointer; const AWidth, AHeight, AComponents: Integer;
  const AData: Pointer): LongBool; cdecl;
  external STB_LIB name _PU + 'stbi_write_hdr_to_func';

{ Writes an image in Jpeg format using a write function.

  Parameters:
    AWriteFunc: to function that is called to write the data
    AUserData: user-defined data that will be passed to the write function.
    AWidth: width of the image.
    AHeight: height of the image.
    AComponents: number of components in the image.
    AData: pointer to the image data. Must be floating-point data.
    AQuality: the quality level (1-100).

  Returns:
    True on failure, False on success.

  HDR expects linear float data. Since the format is always 32-bit rgb(e) data,
  alpha (if provided) is discarded, and for monochrome data it is replicated
  across all three channels. }
function stbi_write_jpg_to_func(const AWriteFunc: TStbiWriteFunc;
  const AUserData: Pointer; const AWidth, AHeight, AComponents: Integer;
  const AData: Pointer; const AQuality: Integer): LongBool; cdecl;
  external STB_LIB name _PU + 'stbi_write_jpg_to_func';

{ Whether to vertically flip images when writing. }
procedure stbi_flip_vertically_on_write(const AFlip: LongBool); cdecl;
  external STB_LIB name _PU + 'stbi_flip_vertically_on_write';

implementation

end.
