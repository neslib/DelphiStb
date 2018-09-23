unit Neslib.Stb.TrueType;
{< stb_truetype: parse, decode, and rasterize characters from truetype fonts.

  Simple 3D API (don't ship this, but it's fine for tools and quick start):
  * stbtt_BakeFontBitmap -- bake a font to a bitmap for use as texture
  * stbtt_GetBakedQuad   -- compute quad to draw for a given char

  Improved 3D API (more shippable):
  * stbtt_PackBegin
  * stbtt_PackSetOversampling -- for improved quality on small fonts
  * stbtt_PackFontRanges      -- pack and renders
  * stbtt_PackEnd
  * stbtt_GetPackedQuad

  "Load" a font file from a memory buffer (you have to keep the buffer loaded):
  * stbtt_InitFont
  * stbtt_GetFontOffsetForIndex -- indexing for TTC font collections
  * stbtt_GetNumberOfFonts()    -- number of fonts for TTC font collections

  Render a unicode codepoint to a bitmap:
  * stbtt_GetCodepointBitmap    -- allocates and returns a bitmap
  * stbtt_MakeCodepointBitmap   -- renders into bitmap you provide
  * stbtt_GetCodepointBitmapBox -- how big the bitmap must be

  Character advance/positioning:
  * stbtt_GetCodepointHMetrics
  * stbtt_GetFontVMetrics
  * stbtt_GetFontVMetricsOS2
  * stbtt_GetCodepointKernAdvance

  Starting with version 1.06, the rasterizer was replaced with a new, faster and
  generally-more-precise rasterizer. The new rasterizer more accurately measures
  pixel coverage for anti-aliasing, except in the case where multiple shapes
  overlap, in which case it overestimates the AA pixel coverage. Thus,
  anti-aliasing of intersecting shapes may look wrong.

  @bold(Important Concepts)

  Some important concepts to understand to use this library:

  * Codepoint: Characters are defined by unicode codepoints, e.g. 65 is
    uppercase A, 231 is lowercase c with a cedilla, $7e30 is the hiragana for
    "ma".
  * Glyph: A visual character shape (every codepoint is rendered as some glyph)
  * Glyph index: A font-specific integer ID representing a glyph
  * Baseline: Glyph shapes are defined relative to a baseline, which is the
    bottom of uppercase characters. Characters extend both above and below the
    baseline.
  * Current Point: As you draw text to the screen, you keep track of a "current
    point" which is the origin of each character. The current point's vertical
    position is the baseline. Even "baked fonts" use this model.
  * Vertical Font Metrics: The vertical qualities of the font, used to
    vertically position and space the characters. See stbtt_GetFontVMetrics.
  * Font Size in Pixels or Points: The preferred interface for specifying font
    sizes in stb_truetype is to specify how tall the font's vertical extent
    should be in pixels. If that sounds good enough, skip the next paragraph.

    Most font APIs instead use "points", which are a common typographic
    measurement for describing font size, defined as 72 points per inch.
    stb_truetype provides a point API for compatibility. However, true
    "per inch" conventions don't make much sense on computer displays
    since different monitors have different number of pixels per inch. For
    example, Windows traditionally uses a convention that there are 96 pixels
    per inch, thus making 'inch' measurements have nothing to do with inches,
    and thus effectively defining a point to be 1.333 pixels. Additionally, the
    TrueType font data provides an explicit scale factor to scale a given font's
    glyphs to points, but the author has observed that this scale factor is
    often wrong for non-commercial fonts, thus making fonts scaled in points
    according to the TrueType spec incoherently sized in practice.

  @Detailed Usage)

 DETAILED USAGE:

 * Scale: Select how high you want the font to be, in points or pixels. Call
   ScaleForPixelHeight or ScaleForMappingEmToPixels to compute a scale factor SF
   that will be used by all other functions.

 * Baseline: You need to select a y-coordinate that is the baseline of where
   your text will appear. Call GetFontBoundingBox to get the baseline-relative
   bounding box for all characters. SF*-y0 will be the distance in pixels that
   the worst-case character could extend above the baseline, so if you want the
   top edge of characters to appear at the top of the screen where y=0, then you
   would set the baseline to SF*-y0.

 * Current point: Set the current point where the first character will appear.
   The first character could extend left of the current point; this is font
   dependent. You can either choose a current point that is the leftmost point
   and hope, or add some padding, or check the bounding box or left-side-bearing
   of the first character to be displayed and set the current point based on
   that.

 * Displaying a character: Compute the bounding box of the character. It will
   contain signed values relative to <current_point, baseline>. I.e. if it
   returns x0,y0,x1,y1,  then the character should be displayed in the rectangle
   from <current_point+SF*x0, baseline+SF*y0> to
   <current_point+SF*x1,baseline+SF*y1).

  * Advancing for the next character: Call GlyphHMetrics, and compute
    'current_point += SF * advance'.

  @bold(Advanced Usage)

  Quality:
  * Use the functions with <tt>Subpixel</tt> at the end to allow your characters
    to have subpixel positioning. Since the font is anti-aliased, not hinted,
    this is very important for quality. (This is not possible with baked fonts.)
  * Kerning is now supported, and if you're supporting subpixel rendering
    then kerning is worth using to give your text a polished look.

  Performance:
  * Convert Unicode codepoints to glyph indexes and operate on the glyphs;
    if you don't do this, stb_truetype is forced to do the conversion on
    every call.
  * There are a lot of memory allocations. We should modify it to take
    a temp buffer and allocate from the temp buffer (without freeing),
    should help performance a lot.

  @bold(Notes)

  The system uses the raw data found in the .ttf file without changing it
  and without building auxiliary data structures. This is a bit inefficient
  on little-endian systems (the data is big-endian), but assuming you're
  caching the bitmaps or glyph shapes this shouldn't be a big deal.

  It appears to be very hard to programmatically determine what font a
  given file is in a general way. I provide an API for this, but I don't
  recommend it. }

interface

uses
  Neslib.Stb.Common,
  Neslib.Stb.RectPack;

type
  { Private structure }
  TStbttBuf = record
    Data: Pointer;
    Cursor: Integer;
    Size: Integer;
  end;

(*** Texture Baking - Simple API ***)

type
  { Baked character information }
  TStbttBakedChar = record
    { Coordinates of the bounding box in the bitmap }
    Left, Top, Right, Bottom: UInt16;

    { Horizontal and vertical offset }
    XOffset, YOffset: Single;

    { Amount to advance horizontally to next glyph }
    XAdvance: Single;
  end;
  PStbttBakedChar = ^TStbttBakedChar;

{ Bake a font to a bitmap for use as texture.

  Parameters:
    ATtfData: pointer to the TTF data.
    AOffset: offset in ATtfData to the start of the TTF data. Use 0 for plain
      .ttf data.
    APixelHeight: height of the font in pixels.
    APixels: pointer to the bitmap data to be filled in. See remarks.
    APixelsWidth: width of the APixels buffer
    APixelsHeight: height of the APixels buffer.
    AFirstChar: codepoint of the first character to bake.
    ANumChars: number of characters to bake.
    ABakedChars: pointer to an array of TStbttBakedChar elements that will be
      filled with information about each baked character. You need to makesure
      the array is at least ANumChars elements long. You can pass these later to
      stbtt_GetBakedQuad.

  Returns:
    * Positive value: the number of the first unused row in the bitmap.
    * Negative value: the negative value of the number of characters that fit.
    * 0: no characters fit and no rows were used.

  APixels must point to a monochrome bitmap (8 bits per pixel) that is at least
  APixelsWidth * APixelsHeight bytes in size.

  This function uses very crappy packing and is thus not for production code.
  But it's fine for tools and quick start. }
function stbtt_BakeFontBitmap(const ATtfData: Pointer; const AOffset: Integer;
  const APixelHeight: Single; const APixels: Pointer; const APixelsWidth,
  APixelsHeight, AFirstChar, ANumChars: Integer;
  const ABakedChars: PStbttBakedChar): Integer; cdecl;
  external STB_LIB name _PU + 'stbtt_BakeFontBitmap';

type
  { An aligned quad }
  TStbttAlignedQuad = record
    { Top-left corner in pixel coordinates }
    X0, Y0: Single;

    { Top-left corner in texture coordinates (0.0 - 1.0) }
    S0, T0: Single;

    { Bottom-right corner in pixel coordinates }
    X1, Y1: Single;

    { Bottom-right corner in texture coordinates (0.0 - 1.0) }
    S1, T1: Single;
  end;
  PStbttAlignedQuad = ^TStbttAlignedQuad;

{ Gets the quad to render a character.

  Parameters:
    ABakedChars: pointer to an array of baked characters passed to
      stbtt_BakeFontBitmap.
    AAtlasWidth: width of the atlas bitmap
    AAtlasHeight: width of the atlas bitmap
    ACharIndex: index of the character in ABakedChars to get the quad for.
    AXPos: current X-position in pixel coordinates. Is updated to the position
      for the next character.
    AYPos: current Y-position in pixel coordinates. Is updated to the position
      for the next character.
    AQuad: is set to the quad to draw.
    AOpenGLFillRule: set to True if you are going to render the quad using
      OpenGL, or False if using DirectX 9 or earlier.

  The coordinate system used assumes Y increases downwards.

  Characters will extend both above and below the current position; see
  discussion of "baseline" in the unit documentation.

  This function is inefficient and is thus not for production code.
  But it's fine for tools and quick start. }
procedure stbtt_GetBakedQuad(const ABakedChars: PStbttBakedChar;
  const AAtlasWidth, AAtlasHeight, ACharIndex: Integer; var AXPos, AYPos: Single;
  out AQuad: TStbttAlignedQuad; const AOpenGLFillRule: LongBool); cdecl;
  external STB_LIB name _PU + 'stbtt_GetBakedQuad';

(*** Texture Baking - Improved API ***)

type
  { Packed character information }
  TStbttPackedChar = record
    { Coordinates of the bounding box in the bitmap }
    Left, Top, Right, Bottom: UInt16;

    { Horizontal and vertical offset }
    XOffset, YOffset: Single;

    { Amount to advance horizontally to next glyph }
    XAdvance: Single;

    { Additional offsets. Used internally. }
    XOffset2, YOffset2: Single;
  end;
  PStbttPackedChar = ^TStbttPackedChar;

type
  { This is an opaque structure that you shouldn't mess with which holds
    all the context needed from stbtt_PackBegin to stbtt_PackEnd. }
  TStbttPackContext = record
    UserAllocatorContext: Pointer;
    PackInfo: Pointer;
    Width: Integer;
    Height: Integer;
    StrideInBytes: Integer;
    Padding: Integer;
    HOversample: Integer;
    VOversample: Integer;
    Pixels: Pointer;
    Nodes: Pointer;
  end;
  PStbttPackContext = ^TStbttPackContext;

type
  { The following structure is defined publically so you can declare one on
    the stack or as a global or etc, but you should treat it as opaque. }
  TStbttFontInfo = record
    UserData: Pointer;
    Data: Pointer;             // pointer to .ttf file
    FontStart: Integer;        // offset of start of font

    NumGlyphs: Integer;        // number of glyphs, needed for range checking

    // table locations as offset from start of .ttf
    Loca, Head, Glyf, Hhea, Hmtx, Kern, Gpos: Integer;

    IndexMap: Integer;         // a cmap mapping for our chosen character encoding
    IndexToLocFormat: Integer; // format needed to map from glyph index to glyph

    Cff: TStbttBuf;            // cff font data
    CharStrings: TStbttBuf;    // the charstring index
    Gsubrs: TStbttBuf;         // global charstring subroutines index
    Subrs: TStbttBuf;          // private charstring subroutines index
    FontDicts: TStbttBuf;      // array of font dicts
    Fdselect: TStbttBuf;       // map from glyph to fontdict
  end;
  PStbttFontInfo = ^TStbttFontInfo;

{ Initializes a packing context stored in the passed-in context.

  Parameters:
    AContext: the context to initialize for future packing.
    APixels: pointer to a 1-channel (8-bit) bitmap to store the glyphs.
    AWidth: width of the bitmap.
    AHeight: height of the bitmap.
    AStrideInBytes: the distance from one row to the next (or 0 to mean they are
      packed tightly together).
    APadding: the amount of padding to leave between each character (normally
      you want 1 for bitmaps you'll use as textures with bilinear filtering).
    AReserved: (optional) pointer that should be nil.

  Returns:
    True on success. False on failure.

  You must free the context at some later point using stbtt_PackEnd. }
function stbtt_PackBegin(out AContext: TStbttPackContext; const APixels: Pointer;
  const AWidth, AHeight, AStrideInBytes, APadding: Integer;
  const AReserved: Pointer = nil): LongBool; cdecl;
  external STB_LIB name _PU + 'stbtt_PackBegin';

{ Cleans up the packing context and frees all memory.

  Parameters:
    AContext: the context to clean up }
procedure stbtt_PackEnd(var AContext: TStbttPackContext); cdecl;
  external STB_LIB name _PU + 'stbtt_PackEnd';

{ Converts point size for use with stbtt_PackFontRange.

  AParameters:
    ASize: value to convert.

  Returns:
    Converted value. }
function StbttPointSize(const AValue: Single): Single; inline;

{ Creates character bitmaps.

  Parameters:
    AContext: the pack context.
    AFontData: pointer to the TTF font data.
    AFontIndex: index of the font in AFontData to use. Pass 0 if you don't know
      what it is.
    AFontSize: the full height of the character from ascender to descender, as
      computed by stbtt_ScaleForPixelHeight. See remarks.
    AFirstCodepointInRange: first codepoint to create a bitmap for.
    ANumCodepointsInRange: number of codepoints to create bitmaps for.
    APackedChars: pointer to an array of TStbttPackedChar elements that will be
      filled with information about each packed character. You need to make sure
      the array is at least ANumCodepointsInRange elements long. You pass these
      later to stbtt_GetPackedQuad to get back renderable quads.

  Returns:
    True on success. False on failure.

  To use a point size as computed by stbtt_ScaleForMappingEmToPixels, wrap the
  point size in StbttPointSize and pass that result as AFontSize.

  Note that you can call this function multiple times within a single
  stbtt_PackBegin/stbtt_PackEnd. }
function stbtt_PackFontRange(var AContext: TStbttPackContext;
  const AFontData: Pointer; const AFontIndex: Integer; const AFontSize: Single;
  const AFirstCodepointInRange, ANumCodepointsInRange: Integer;
  const APackedChars: PStbttPackedChar): LongBool; cdecl;
  external STB_LIB name _PU + 'stbtt_PackFontRange';

type
  { A font packing range. Used with stbtt_PackFontRange to pack multiple ranges
    in one call. }
  TStbttPackRange = record
    { The full height of the character from ascender to descender, as
      computed by stbtt_ScaleForPixelHeight }
    FontSize: Single;

    { First codepoint in the range. If non-zero then the range is continuous,
      starting at this codepoint. Otherwise, then the Codepoints field is
      used instead. }
    FirstCodepointInRange: Integer;

    { Pointer to an array of codepoints to pack. Can be nil to pack a sequential
      range of codepoints (in that case FirstCodepointInRange must > 0).
      Otherwiwe, it must be large enougn to hold at least NumChars elements (and
      FirstCodepointInRange must be set to 0). }
    Codepoints: PInteger;

    { Number of characters to pack. If Codepoints is nil, then NumChars
      codepoints starting at FirstCodepointInRange are packed. Otherwise,
      NumChars must be the number if elements in the Codepoints array. }
    NumChars: Integer;

    { Output array of TStbttPackedChar elements that will be filled with
      information about each packed character. You need to make sure the array
      is at least NumChars elements long. You pass these later to
      stbtt_GetPackedQuad to get back renderable quads. }
    PackedChars: PStbttPackedChar;

    { Internal fields. Do not use these }
    _HOversample, _VOversample: Byte;
  end;
  PStbttPackRange = ^TStbttPackRange;

{ Creates character bitmaps for multiple ranges.

  Parameters:
    AContext: the pack context.
    AFontData: pointer to the TTF font data.
    AFontIndex: index of the font in AFontData to use. Pass 0 if you don't know
      what it is.
    ARanges: pointer to an array of TStbttPackRange elements describing each
      range to pack.
    ANumRanges: the number of elements in the ARanges array.

  Returns:
    True on success. False on failure.

  This version will usually create a better-packed bitmap than multiple
  calls to stbtt_PackFontRange. Note that you can call this multiple times
  within a single stbtt_PackBegin/stbtt_PackEnd. }
function stbtt_PackFontRanges(var AContext: TStbttPackContext;
  const AFontData: Pointer; const AFontIndex: Integer;
  const ARanges: PStbttPackRange; const ANumRanges: Integer): LongBool; cdecl;
  external STB_LIB name _PU + 'stbtt_PackFontRanges';

{ This function sets the amount of oversampling for all following calls to
  stbtt_PackFontRange(s) or stbtt_PackFontRangesGatherRects for a given
  pack context.

  Parameters:
    AContext: the pack context.
    AHOversample: amount of oversampling in horizontal direction.
    AVOversample: amount of oversampling in vertical direction.

  Oversampling a font increases the quality by allowing higher-quality subpixel
  positioning, and is especially valuable at smaller text sizes.

  The default (no oversampling) is achieved by AHOversample=1 and
  AVoversample=1. The total number of pixels required is
  HOversample * AVOversample larger than the default; for example, 2x2
  oversampling requires 4x the storage of 1x1. For best results, render
  oversampled textures with bilinear filtering.

  To use with PackFontRangesGather etc., you must set it before calls
  call to PackFontRangesGatherRects. }
procedure stbtt_PackSetOversampling(var AContext: TStbttPackContext;
  const AHOversample, AVOversample: Single); cdecl;
  external STB_LIB name _PU + 'stbtt_PackSetOversampling';

{ Gets the quad to render a character.

  Parameters:
    APackedChars: pointer to an array of baked characters passed to
      stbtt_PackFontRange(s).
    AAtlasWidth: width of the atlas bitmap
    AAtlasHeight: width of the atlas bitmap
    ACharIndex: index of the character in APackedChars to get the quad for.
    AXPos: current X-position in pixel coordinates. Is updated to the position
      for the next character.
    AYPos: current Y-position in pixel coordinates. Is updated to the position
      for the next character.
    AQuad: is set to the quad to draw.
    AAlignToInteger: set to True to align the character to an integer pixel
      grid.

  The coordinate system used assumes Y increases downwards.

  Characters will extend both above and below the current position; see
  discussion of "baseline" in the unit documentation. }
procedure stbtt_GetPackedQuad(const APackedChars: PStbttPackedChar;
  const AAtlasWidth, AAtlasHeight, ACharIndex: Integer; var AXPos, AYPos: Single;
  out AQuad: TStbttAlignedQuad; const AAlignToInteger: LongBool); cdecl;
  external STB_LIB name _PU + 'stbtt_GetPackedQuad';

{ Calling these functions in sequence is roughly equivalent to calling
  stbtt_PackFontRanges. If you more control over the packing of multiple
  fonts, or if you want to pack custom data into a font texture, take a look
  at the source to of stbtt_PackFontRanges() and create a custom version using
  these functions, e.g. call stbtt_PackFontRangesGatherRects multiple times,
  building up a single array of rects, then call stbtt_PackFontRangesPackRects
  once, then call stbtt_PackFontRangesRenderIntoRects repeatedly. This may
  result in a better packing than calling PackFontRanges multiple times
  (or it may not). }
function stbtt_PackFontRangesGatherRects(var AContext: TStbttPackContext;
  var AInfo: TStbttFontInfo; const ARanges: PStbttPackRange;
  const ANumRanges: Integer; const ARects: PStbrpRect): Integer; cdecl;
  external STB_LIB name _PU + 'stbtt_PackFontRangesGatherRects';
procedure stbtt_PackFontRangesPackRects(var AContext: TStbttPackContext;
  const ARects: PStbrpRect; const ANumRects: Integer); cdecl;
  external STB_LIB name _PU + 'stbtt_PackFontRangesPackRects';
function stbtt_PackFontRangesRenderIntoRects(var AContext: TStbttPackContext;
  var AInfo: TStbttFontInfo; const ARanges: PStbttPackRange;
  const ANumRanges: Integer; const ARects: PStbrpRect): LongBool; cdecl;
  external STB_LIB name _PU + 'stbtt_PackFontRangesRenderIntoRects';

(*** Font Loang ***)

{ This function will determine the number of fonts in a font file.

  Parameters:
    ATtfData: pointer to the TTF data.

  Returns:
    The number of fonts.

  TrueType collection (.ttc) files may contain multiple fonts, while TrueType
  font (.ttf) files only contain one font. The number of fonts can be used for
  indexing with the previous function where the index is between zero and one
  less than the total fonts. If an error occurs, -1 is returned. }
function stbtt_GetNumberOfFonts(const ATtfData: Pointer): Integer; cdecl;
  external STB_LIB name _PU + 'stbtt_GetNumberOfFonts';

{ Get the offset of a font in a .ttf/.ttc file at a given index.

  Parameters:
    ATtfData: pointer to the TTF data.
    AIndex: index of the font.

  Returns:
    Offset of the font, or -1 if the index is out of range.

  Each .ttf/.ttc file may have more than one font. Each font has a sequential
  index number starting from 0. Call this function to get the font offset for
  a given index.

  A regular .ttf file will only define one font and it always be at offset 0, so
  it will return '0' for index 0, and -1 for all other indices. }
function stbtt_GetFontOffsetForIndex(const ATtfData: Pointer;
  const AIndex: Integer): Integer; cdecl;
  external STB_LIB name _PU + 'stbtt_GetFontOffsetForIndex';

{ Given an offset into the file that defines a font, this function builds
  the necessary cached info for the rest of the system.

  Parameters:
    AInfo: will be filled out with the font information.
    ATtfData: pointer to the TTF data.
    AOffset: offset in ATtfData to the start of the TTF data. Use 0 for plain
      .ttf data.

  Returns:
    True on success. False on failure. }
function stbtt_InitFont(var AInfo: TStbttFontInfo; const ATtfData: Pointer;
  const AOffset: Integer): LongBool; cdecl;
  external STB_LIB name _PU + 'stbtt_InitFont';

{ Converts a Unicode codepoint to a glyph index in a font.

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    ACodepoint: the Unicode codepoint to convert.

  Returns:
    The glyph index for the codepoint.

  If you're going to perform multiple operations on the same character and you
  want a speed-up, call this function with the character you're going to
  process, then use glyph-based functions instead of the codepoint-based
  functions. }
function stbtt_FindGlyphIndex(var AInfo: TStbttFontInfo;
  const ACodepoint: Integer): Integer; cdecl;
  external STB_LIB name _PU + 'stbtt_FindGlyphIndex';

{ Computes a scale factor to produce a font whose "height" is APixels tall.

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    APixels: requested pixel height.

  Returns:
    Scale factor.

  Height is measured as the distance from the highest ascender to the lowest
  descender; in other words, it's equivalent to calling stbtt_GetFontVMetrics
  and computing: <tt>Scale := APixels / (Ascent - Descent)</tt>, so if you
  prefer to measure height by the ascent only, use a similar calculation. }
function stbtt_ScaleForPixelHeight(var AInfo: TStbttFontInfo;
  const APixels: Single): Single; cdecl;
  external STB_LIB name _PU + 'stbtt_ScaleForPixelHeight';

{ Computes a scale factor to produce a font whose EM size is mapped to
  APixels tall.

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    APixels: requested pixel height.

  Returns:
    Scale factor.

  This is probably what traditional APIs compute, but I'm not positive. }
function stbtt_ScaleForMappingEmToPixels(var AInfo: TStbttFontInfo;
  const APixels: Single): Single; cdecl;
  external STB_LIB name _PU + 'stbtt_ScaleForMappingEmToPixels';

{ Retrieves vertical font metrics about a font.

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    AAscent: is set to the coordinate above the baseline the font extends.
    ADescent: is set to the coordinate below the baseline the font extends (i.e.
      it is typically negative).
    ALineGap: is set to the spacing between one row's descent and the next row's
      ascent.

  You should advance the vertical position by <tt>AAscent - ADescent + ALineGap</tt>.
  These are expressed in unscaled coordinates, so you must multiply by the
  scale factor for a given size. }
procedure stbtt_GetFontVMetrics(var AInfo: TStbttFontInfo;
  out AAscent, ADescent, ALineGap: Integer); cdecl;
  external STB_LIB name _PU + 'stbtt_GetFontVMetrics';

{ Analogous to GetFontVMetrics, but returns the "typographic" values from the
  OS/2 table (specific to MS/Windows TTF files).

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    ATypeAscent: is set to the coordinate above the baseline the font extends.
    ATypeDescent: is set to the coordinate below the baseline the font extends
      (i.e. it is typically negative).
    ATypeLineGap: is set to the spacing between one row's descent and the next
      row's ascent.

  Returns:
    True on success (table present), False on failure. }
function stbtt_GetFontVMetricsOS2(var AInfo: TStbttFontInfo;
  out ATypoAscent, ATypoDescent, ATypoLineGap: Integer): LongBool; cdecl;
  external STB_LIB name _PU + 'stbtt_GetFontVMetricsOS2';

{ Retrieves the bounding box around all possible characters;

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    ALeft: is set to the left edge of the bounding box.
    ATop: is set to the top edge of the bounding box.
    ARight: is set to the right edge of the bounding box.
    ABottom: is set to the bottom edge of the bounding box. }
procedure stbtt_GetFontBoundingBox(var AInfo: TStbttFontInfo;
  out ALeft, ATop, ARight, ABottom: Integer); cdecl;
  external STB_LIB name _PU + 'stbtt_GetFontBoundingBox';

{ Retrieves horizontal font metrics about a codepoint in a font.

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    ACodepoint: the codepoint to retrieve metrics for.
    AAdvanceWidth: the offset from the current horizontal position to the next
      horizontal position.
    ALeftSideBearing: the offset from the current horizontal position to the
      left edge of the character.

  These are expressed in unscaled coordinates.

  If you perform multiple operations on the same codepoint, it is more efficient
  to use stbtt_FindGlyphIndex in combination with stbtt_GetGlyphHMetrics. }
procedure stbtt_GetCodepointHMetrics(var AInfo: TStbttFontInfo;
  const ACodepoint: Integer; out AAdvanceWidth, ALeftSideBearing: Integer); cdecl;
  external STB_LIB name _PU + 'stbtt_GetCodepointHMetrics';

{ Retrieves the kerning value for a pair of codepoints.

  Parameters:
    ACodepoint1: first codepoint
    ACodepoint2: second codepoint

  Returns:
    An additional amount to add to the "advance" value between ACodepoint1
    and ACodepoint2.

  If you perform multiple operations on the same codepoint, it is more efficient
  to use stbtt_FindGlyphIndex in combination with stbtt_GetGlyphKernAdvance. }
function stbtt_GetCodepointKernAdvance(var AInfo: TStbttFontInfo;
  const ACodepoint1, ACodepoint2: Integer): Integer; cdecl;
  external STB_LIB name _PU + 'stbtt_GetCodepointKernAdvance';

{ Gets the bounding box of the visible part of the glyph, in unscaled
  coordinates.

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    ACodepoint: the codepoint
    ALeft: is set to the left edge of the bounding box.
    ATop: is set to the top edge of the bounding box.
    ARight: is set to the right edge of the bounding box.
    ABottom: is set to the bottom edge of the bounding box.

  Returns:
    True on success. False on failure.

  If you perform multiple operations on the same codepoint, it is more efficient
  to use stbtt_FindGlyphIndex in combination with stbtt_GetGlyphBox. }
function stbtt_GetCodepointBox(var AInfo: TStbttFontInfo; const ACodepoint: Integer;
  out ALeft, ATop, ARight, ABottom: Integer): Longbool; cdecl;
  external STB_LIB name _PU + 'stbtt_GetCodepointBox';

{ Retrieves horizontal font metrics about a glyph in a font.

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    AGlyphIndex: the glyph to retrieve metrics for.
    AAdvanceWidth: the offset from the current horizontal position to the next
      horizontal position.
    ALeftSideBearing: the offset from the current horizontal position to the
      left edge of the character.

  These are expressed in unscaled coordinates.

  This is the glyph-based version of stbtt_GetCodepointHMetrics. }
procedure stbtt_GetGlyphHMetrics(var AInfo: TStbttFontInfo;
  const AGlyphIndex: Integer; out AAdvanceWidth, ALeftSideBearing: Integer); cdecl;
  external STB_LIB name _PU + 'stbtt_GetGlyphHMetrics';

{ Retrieves the kerning value for a pair of glyphs.

  Parameters:
    AGlyphIndex1: first codepoint
    AGlyphIndex2: second codepoint

  Returns:
    An additional amount to add to the "advance" value between AGlyphIndex1
    and AGlyphIndex2.

  This is the glyph-based version of stbtt_GetCodepointKernAdvance. }
function stbtt_GetGlyphKernAdvance(var AInfo: TStbttFontInfo;
  const AGlyphIndex1, AGlyphIndex2: Integer): Integer; cdecl;
  external STB_LIB name _PU + 'stbtt_GetGlyphKernAdvance';

{ Gets the bounding box of the visible part of the glyph, in unscaled
  coordinates.

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    AGlyphIndex: the glyph index.
    ALeft: is set to the left edge of the bounding box.
    ATop: is set to the top edge of the bounding box.
    ARight: is set to the right edge of the bounding box.
    ABottom: is set to the bottom edge of the bounding box.

  Returns:
    True on success. False on failure.

  This is the glyph-based version of stbtt_GetCodepointBox. }
function stbtt_GetGlyphBox(var AInfo: TStbttFontInfo; const AGlyphIndex: Integer;
  out ALeft, ATop, ARight, ABottom: Integer): Longbool; cdecl;
  external STB_LIB name _PU + 'stbtt_GetGlyphBox';

{ Checks whether a glyph is empty.

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    AGlyphIndex: the glyph index.

  Returns:
    True if there is nothing to be drawn for the glyph. }
function stbtt_IsGlyphEmpty(var AInfo: TStbttFontInfo;
  const AGlyphIndex: Integer): Longbool; cdecl;
  external STB_LIB name _PU + 'stbtt_IsGlyphEmpty';

type
  { Operation on a TStbttVertex }
  TStbttVertexOperation = (
    { Move to the vertex position }
    MoveTo = 1,

    { Draw a line to the vertex position }
    LineTo = 2,

    { Draw a curve to the vertex position }
    CurveTo = 3);

type
  { Defines a vertex as part of a glyph shape }
  TStbttVertex = record
    { Position of the vertex }
    X, Y: Int16;

    { Curve control point }
    CX, CY: Int16;

    { Operation to use for the vertex }
    Operation: TStbttVertexOperation;

    { Padding }
    Padding: Byte;
  end;
  PStbttVertex = ^TStbttVertex;

{ Retrieves the shape of a codepoint.

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    ACodepoint: the codepoint.
    AVertices: is set to an array of vertices that define the shape.

  Returns:
    The number of vertices in AVertices.

  The shape is a series of countours. Each one starts with a MoveTo operation,
  then consists of a series of mixed LineTo and CurveTo segments. A LineTo
  draws a line from previous endpoint to its X, Y. A CurveTo draws a quadratic
  bezier from previous endpoint to its X, Y, using CX, CY as the bezier control
  point.

  You must call stbtt_FreeShape to free the shape later.

  If you perform multiple operations on the same codepoint, it is more efficient
  to use stbtt_FindGlyphIndex in combination with stbtt_GetGlyphShape. }
function stbtt_GetCodepointShape(var AInfo: TStbttFontInfo;
  const ACodepoint: Integer; out AVertices: PStbttVertex): Integer; cdecl;
  external STB_LIB name _PU + 'stbtt_GetCodepointShape';

{ Retrieves the shape of a glyph index.

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    AGlyphIndex: the glyph index.
    AVertices: is set to an array of vertices that define the shape.

  Returns:
    The number of vertices in AVertices.

  The shape is a series of countours. Each one starts with a MoveTo operation,
  then consists of a series of mixed LineTo and CurveTo segments. A LineTo
  draws a line from previous endpoint to its X, Y. A CurveTo draws a quadratic
  bezier from previous endpoint to its X, Y, using CX, CY as the bezier control
  point.

  You must call stbtt_FreeShape to free the shape later.

  This is the glyph-based version of stbtt_GetCodepointShape. }
function stbtt_GetGlyphShape(var AInfo: TStbttFontInfo;
  const AGlyphIndex: Integer; out AVertices: PStbttVertex): Integer; cdecl;
  external STB_LIB name _PU + 'stbtt_GetGlyphShape';

{ Frees the shape returned by stbtt_GetCodepointShape or stbtt_GetGlyphShape.

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    AVertices: the vertices to free. }
procedure stbtt_FreeShape(var AInfo: TStbttFontInfo;
  const AVertices: PStbttVertex); cdecl;
  external STB_LIB name _PU + 'stbtt_FreeShape';

{ Frees a bitmap created with stbtt_GetCodepointBitmap*, or
  stbtt_GetGlyphBitmap*.

  Parameters:
    ABitmap: the bitmap returned from stbtt_GetCodepointBitmap*, or
      stbtt_GetGlyphBitmap*.
    AUserData: user-defined pointer. Usually nil. }
procedure stbtt_FreeBitmap(const ABitmap, AUserData: Pointer); cdecl;
  external STB_LIB name _PU + 'stbtt_FreeBitmap';

{ Allocates a large-enough single-channel 8bpp bitmap and renders the
  specified character/glyph at the specified scale into it, with
  antialiasing (0 is no coverage (transparent), 255 is fully covered (opaque)).

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    AXScale: horizontal scale factor
    AYScale: vertical scale factor
    ACodepoint: codepoint to render.
    AWidth: is set to the width of the bitmap
    AHeight: is set to the height of the bitmap
    AXOffset: is set to the horizontal offset in pixel space from the glyph
      origin to the left edge of the bitmap.
    AYOffset: is set to the vertical offset in pixel space from the glyph
      origin to the top edge of the bitmap.

  Returns:
    The bitmap. You must free this later with stbtt_FreeBitmap.

  If you perform multiple operations on the same codepoint, it is more efficient
  to use stbtt_FindGlyphIndex in combination with stbtt_GetGlyphBitmap. }
function stbtt_GetCodepointBitmap(var AInfo: TStbttFontInfo;
  const AXScale, AYScale: Single; const ACodepoint: Integer; out AWidth, AHeight,
  AXOffset, AYOffset: Integer): Pointer; cdecl;
  external STB_LIB name _PU + 'stbtt_GetCodepointBitmap';

{ As stbtt_GetCodepointBitmap, but you can specify a subpixel shift for the
  character.

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    AXScale: horizontal scale factor
    AYScale: vertical scale factor
    AXShift: horizontal subpixel shift
    AYShift: vertical subpixel shift
    ACodepoint: codepoint to render.
    AWidth: is set to the width of the bitmap
    AHeight: is set to the height of the bitmap
    AXOffset: is set to the horizontal offset in pixel space from the glyph
      origin to the left edge of the bitmap.
    AYOffset: is set to the vertical offset in pixel space from the glyph
      origin to the top edge of the bitmap.

  Returns:
    The bitmap. You must free this later with stbtt_FreeBitmap.

  If you perform multiple operations on the same codepoint, it is more efficient
  to use stbtt_FindGlyphIndex in combination with stbtt_GetGlyphBitmapSubpixel. }
function stbtt_GetCodepointBitmapSubpixel(var AInfo: TStbttFontInfo;
  const AXScale, AYScale, AXShift, AYShift: Single; const ACodepoint: Integer;
  out AWidth, AHeight, AXOffset, AYOffset: Integer): Pointer; cdecl;
  external STB_LIB name _PU + 'stbtt_GetCodepointBitmapSubpixel';

{ As stbtt_GetCodepointBitmap, but you pass in storage for the bitmap.

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    ABitmap: pointer to the bitmap to store the glyph shape.
    ABitmapWidth: width of the bitmap.
    ABitmapHeight: height of the bitmap.
    ABitmapStride: distance from one row in the bitmap to the next (in bytes).
    AXScale: horizontal scale factor
    AYScale: vertical scale factor
    ACodepoint: codepoint to render.

  Call stbtt_GetCodepointBitmapBox to get the width and height and positioning
  info for it first.

  If you perform multiple operations on the same codepoint, it is more efficient
  to use stbtt_FindGlyphIndex in combination with stbtt_MakeGlyphBitmap. }
procedure stbtt_MakeCodepointBitmap(var AInfo: TStbttFontInfo;
  const ABitmap: Pointer; const ABitmapWidth, ABitmapHeight, ABitmapStride: Integer;
  const AXScale, AYScale: Single; const ACodepoint: Integer); cdecl;
  external STB_LIB name _PU + 'stbtt_MakeCodepointBitmap';

{ As stbtt_MakeCodepointBitmap, but you can specify a subpixel shift for the
  character.

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    ABitmap: pointer to the bitmap to store the glyph shape.
    ABitmapWidth: width of the bitmap.
    ABitmapHeight: height of the bitmap.
    ABitmapStride: distance from one row in the bitmap to the next (in bytes).
    AXScale: horizontal scale factor
    AYScale: vertical scale factor
    AXShift: horizontal subpixel shift
    AYShift: vertical subpixel shift
    ACodepoint: codepoint to render.

  Call stbtt_GetCodepointBitmapBoxSubpixel to get the width and height and
  positioning info for it first.

  If you perform multiple operations on the same codepoint, it is more efficient
  to use stbtt_FindGlyphIndex in combination with stbtt_MakeGlyphBitmapSubpixel. }
procedure stbtt_MakeCodepointBitmapSubpixel(var AInfo: TStbttFontInfo;
  const ABitmap: Pointer; const ABitmapWidth, ABitmapHeight, ABitmapStride: Integer;
  const AXScale, AYScale, AXShift, AYShift: Single; const ACodepoint: Integer); cdecl;
  external STB_LIB name _PU + 'stbtt_MakeCodepointBitmapSubpixel';

{ Same as stbtt_MakeCodepointBitmapSubpixel, but prefiltering is performed (see
  stbtt_PackSetOversampling) }
procedure stbtt_MakeCodepointBitmapSubpixelPrefilter(var AInfo: TStbttFontInfo;
  const ABitmap: Pointer; const ABitmapWidth, ABitmapHeight, ABitmapStride: Integer;
  const AXScale, AYScale, AXShift, AYShift: Single; const AOversampleX,
  AOversampleY: Integer; out ASubX, ASubY: Single; const ACodepoint: Integer); cdecl;
  external STB_LIB name _PU + 'stbtt_MakeCodepointBitmapSubpixelPrefilter';

{ Get the bounding box of the bitmap centered around the glyph origin.

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    ACodepoint: the codepoint.
    AXScale: horizontal scale factor
    AYScale: vertical scale factor
    ALeft: is set to the left edge of the bounding box.
    ATop: is set to the top edge of the bounding box.
    ARight: is set to the right edge of the bounding box.
    ABottom: is set to the bottom edge of the bounding box.

  The bitmap width is ARight - ALeft, height is ABottom - ATop, and the location
  to place the bitmap top left is (LeftSideBearing * Scale, ATop).

  Note that the bitmap uses Y-increases-down, but the shape uses Y-increases-up,
  so stbtt_GetCodepointBitmapBox and stbtt_GetCodepointBox are inverted.) }
procedure stbtt_GetCodepointBitmapBox(var AInfo: TStbttFontInfo;
  const ACodepoint: Integer; const AXScale, AYScale: Single;
  out ALeft, ATop, ARight, ABottom: Integer); cdecl;
  external STB_LIB name _PU + 'stbtt_GetCodepointBitmapBox';

{ As stbtt_GetCodepointBitmapBox, but you can specify a subpixel shift for the
  character.

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    ACodepoint: the codepoint.
    AXScale: horizontal scale factor
    AYScale: vertical scale factor
    AXShift: horizontal subpixel shift
    AYShift: vertical subpixel shift
    ALeft: is set to the left edge of the bounding box.
    ATop: is set to the top edge of the bounding box.
    ARight: is set to the right edge of the bounding box.
    ABottom: is set to the bottom edge of the bounding box.

  The bitmap width is ARight - ALeft, height is ABottom - ATop, and the location
  to place the bitmap top left is (LeftSideBearing * Scale, ATop).

  Note that the bitmap uses Y-increases-down, but the shape uses Y-increases-up,
  so stbtt_GetCodepointBitmapBoxSubpixel and stbtt_GetCodepointBox are inverted.) }
procedure stbtt_GetCodepointBitmapBoxSubpixel(var AInfo: TStbttFontInfo;
  const ACodepoint: Integer; const AXScale, AYScale, AXShift, AYShift: Single;
  out ALeft, ATop, ARight, ABottom: Integer); cdecl;
  external STB_LIB name _PU + 'stbtt_GetCodepointBitmapBoxSubpixel';

{ Allocates a large-enough single-channel 8bpp bitmap and renders the
  specified character/glyph at the specified scale into it, with
  antialiasing (0 is no coverage (transparent), 255 is fully covered (opaque)).

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    AXScale: horizontal scale factor
    AYScale: vertical scale factor
    AGlyphIndex: index of the glyph to render
    AWidth: is set to the width of the bitmap
    AHeight: is set to the height of the bitmap
    AXOffset: is set to the horizontal offset in pixel space from the glyph
      origin to the left edge of the bitmap.
    AYOffset: is set to the vertical offset in pixel space from the glyph
      origin to the top edge of the bitmap.

  Returns:
    The bitmap. You must free this later with stbtt_FreeBitmap.

  This is the glyph-based version of stbtt_GetCodepointBitmap. }
function stbtt_GetGlyphBitmap(var AInfo: TStbttFontInfo;
  const AXScale, AYScale: Single; const AGlyphIndex: Integer; out AWidth, AHeight,
  AXOffset, AYOffset: Integer): Pointer; cdecl;
  external STB_LIB name _PU + 'stbtt_GetGlyphBitmap';

{ As stbtt_GetGlyphBitmap, but you can specify a subpixel shift for the
  character.

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    AXScale: horizontal scale factor
    AYScale: vertical scale factor
    AXShift: horizontal subpixel shift
    AYShift: vertical subpixel shift
    AGlyphIndex: index of the glyph to render
    AWidth: is set to the width of the bitmap
    AHeight: is set to the height of the bitmap
    AXOffset: is set to the horizontal offset in pixel space from the glyph
      origin to the left edge of the bitmap.
    AYOffset: is set to the vertical offset in pixel space from the glyph
      origin to the top edge of the bitmap.

  Returns:
    The bitmap. You must free this later with stbtt_FreeBitmap.

  This is the glyph-based version of stbtt_GetCodepointBitmapSubpixel. }
function stbtt_GetGlyphBitmapSubpixel(var AInfo: TStbttFontInfo;
  const AXScale, AYScale, AXShift, AYShift: Single; const AGlyphIndex: Integer;
  out AWidth, AHeight, AXOffset, AYOffset: Integer): Pointer; cdecl;
  external STB_LIB name _PU + 'stbtt_GetGlyphBitmapSubpixel';

{ As stbtt_GetGlyphBitmap, but you pass in storage for the bitmap.

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    ABitmap: pointer to the bitmap to store the glyph shape.
    ABitmapWidth: width of the bitmap.
    ABitmapHeight: height of the bitmap.
    ABitmapStride: distance from one row in the bitmap to the next (in bytes).
    AXScale: horizontal scale factor
    AYScale: vertical scale factor
    AGlyphIndex: index of the glyph to render

  Call stbtt_GetGlyphBitmapBox to get the width and height and positioning
  info for it first.

  This is the glyph-based version of stbtt_MakeCodepointBitmap. }
procedure stbtt_MakeGlyphBitmap(var AInfo: TStbttFontInfo;
  const ABitmap: Pointer; const ABitmapWidth, ABitmapHeight, ABitmapStride: Integer;
  const AXScale, AYScale: Single; const AGlyphIndex: Integer); cdecl;
  external STB_LIB name _PU + 'stbtt_MakeGlyphBitmap';

{ As stbtt_MakeGlyphBitmap, but you can specify a subpixel shift for the
  character.

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    ABitmap: pointer to the bitmap to store the glyph shape.
    ABitmapWidth: width of the bitmap.
    ABitmapHeight: height of the bitmap.
    ABitmapStride: distance from one row in the bitmap to the next (in bytes).
    AXScale: horizontal scale factor
    AYScale: vertical scale factor
    AXShift: horizontal subpixel shift
    AYShift: vertical subpixel shift
    AGlyphIndex: index of the glyph to render

  Call stbtt_GetGlyphBitmapBoxSubpixel to get the width and height and
  positioning info for it first.

  This is the glyph-based version of stbtt_MakeCodepointBitmapSubpixel. }
procedure stbtt_MakeGlyphBitmapSubpixel(var AInfo: TStbttFontInfo;
  const ABitmap: Pointer; const ABitmapWidth, ABitmapHeight, ABitmapStride: Integer;
  const AXScale, AYScale, AXShift, AYShift: Single; const AGlyphIndex: Integer); cdecl;
  external STB_LIB name _PU + 'stbtt_MakeGlyphBitmapSubpixel';

{ Same as stbtt_MakeGlyphBitmapSubpixel, but prefiltering is performed (see
  stbtt_PackSetOversampling) }
procedure stbtt_MakeGlyphBitmapSubpixelPrefilter(var AInfo: TStbttFontInfo;
  const ABitmap: Pointer; const ABitmapWidth, ABitmapHeight, ABitmapStride: Integer;
  const AXScale, AYScale, AXShift, AYShift: Single; const AOversampleX,
  AOversampleY: Integer; out ASubX, ASubY: single; const AGlyphIndex: Integer); cdecl;
  external STB_LIB name _PU + 'stbtt_MakeGlyphBitmapSubpixelPrefilter';

{ Get the bounding box of the bitmap centered around the glyph origin.

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    AGlyphIndex: index of the glyph.
    AXScale: horizontal scale factor
    AYScale: vertical scale factor
    ALeft: is set to the left edge of the bounding box.
    ATop: is set to the top edge of the bounding box.
    ARight: is set to the right edge of the bounding box.
    ABottom: is set to the bottom edge of the bounding box.

  The bitmap width is ARight - ALeft, height is ABottom - ATop, and the location
  to place the bitmap top left is (LeftSideBearing * Scale, ATop).

  Note that the bitmap uses Y-increases-down, but the shape uses Y-increases-up,
  so stbtt_GetGlyphBitmapBox and stbtt_GetGlyphBox are inverted.)

  This is the glyph-based version of stbtt_GetCodepointBitmapBox. }
procedure stbtt_GetGlyphBitmapBox(var AInfo: TStbttFontInfo;
  const AGlyphIndex: Integer; const AXScale, AYScale: Single;
  out ALeft, ATop, ARight, ABottom: Integer); cdecl;
  external STB_LIB name _PU + 'stbtt_GetGlyphBitmapBox';

{ As stbtt_GetGlyphBitmapBox, but you can specify a subpixel shift for the
  character.

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    AGlyphIndex: index of the glyph.
    AXScale: horizontal scale factor
    AYScale: vertical scale factor
    AXShift: horizontal subpixel shift
    AYShift: vertical subpixel shift
    ALeft: is set to the left edge of the bounding box.
    ATop: is set to the top edge of the bounding box.
    ARight: is set to the right edge of the bounding box.
    ABottom: is set to the bottom edge of the bounding box.

  The bitmap width is ARight - ALeft, height is ABottom - ATop, and the location
  to place the bitmap top left is (LeftSideBearing * Scale, ATop).

  Note that the bitmap uses Y-increases-down, but the shape uses Y-increases-up,
  so stbtt_GetGlyphBitmapBoxSubpixel and stbtt_GetGlyphBox are inverted.)

  This is the glyph-based version of stbtt_GetCodepointBitmapBoxSubpixel. }
procedure stbtt_GetGlyphBitmapBoxSubpixel(var AInfo: TStbttFontInfo;
  const AGlyphIndex: Integer; const AXScale, AYScale, AXShift, AYShift: Single;
  out ALeft, ATop, ARight, ABottom: Integer); cdecl;
  external STB_LIB name _PU + 'stbtt_GetGlyphBitmapBoxSubpixel';

type
  { Defines a bitmap }
  TStbttBitmap = record
    { Dimensions }
    Width, Height: Integer;

    { Number of bytes from one row to the next }
    Stride: Integer;

    { Pixel data }
    Pixels: Pointer;
  end;
  PStbttBitmap = ^TStbttBitmap;

{ Rasterize a shape with quadratic beziers into a bitmap.

  Parameters:
    ABitmap: 1-channel bitmap to draw into.
    AFlatnessInPixels: allowable error of curve in pixels.
    AVertices: pointer to an array of vertices defining shape.
    ANumVertices: number of vertices in the AVertices array.
    AXScale: horizontal scale applied to input vertices.
    AYScale: vertical scale applied to input vertices.
    AXShift: horizontal translation applied to input vertices.
    AYShift: vertical translation applied to input vertices.
    AXOffset: another horizontal translation applied to input.
    AYOffset: another vertical translation applied to input.
    AFlipVertical: (optional) set to True to vertically flip the shape.
    AReserved: (optional) reserved pointer. Must be nil. }
procedure stbtt_Rasterize(var ABitmap: TStbttBitmap;
  const AFlatnessInPixels: Single; const AVertices: PStbttVertex;
  const ANumVertices: Integer; const AXScale, AYScale, AXShift, AYShift: Single;
  const AXOffset, AYOffset: Integer; const AFlipVertical: LongBool = False;
  const AReserved: Pointer = nil); cdecl;
  external STB_LIB name _PU + 'stbtt_Rasterize';

(** Signed Distance Function (or Field) rendering **)

{ Frees the SDF bitmap allocated below.

  Parameter
    ABitmap: the bitmap returned by stbtt_GetGlyphSDF or stbtt_GetCodepointSDF.
    AUserdata: application defined userdata }
procedure stbtt_FreeSDF(const ABitmap, AUserdata: Pointer); cdecl;
  external STB_LIB name _PU + 'stbtt_FreeSDF';

{ These functions compute a discretized SDF field for a single character,
  suitable for storing in a single-channel texture, sampling with bilinear
  filtering, and testing against larger than some threshhold to produce scalable
  fonts.

  Parameters:
    AInfo: the font
    AScale: controls the size of the resulting SDF bitmap, same as it would be
      creating a regular bitmap
    AGlyph/ACodepoint: the character to generate the SDF for
    APadding: extra "pixels" around the character which are filled with the
      distance to the character (not 0), which allows effects like bit outlines
    AOnEdgeValue: value 0-255 to test the SDF against to reconstruct the
      character (i.e. the isocontour of the character)
    APixelDistScale: what value the SDF should increase by when moving one SDF
      "pixel" away from the edge (on the 0..255 scale). If positive,
      > AOnEdgeValue is inside; if negative, < AOnEdgeValue is inside.
    AWidth, AHeight: output height & width of the SDF bitmap (including padding)
    AXOff, AYOff: output origin of the character

  Returns:
    A 2D array of bytes 0..255, AWidth * AHeight in size

  APixelDistScale & AOnEdgeValue are a scale & bias that allows you to make
  optimal use of the limited 0..255 for your application, trading off precision
  and special effects. SDF values outside the range 0..255 are clamped to
  0..255.

  Example:
    AScale = stbtt_ScaleForPixelHeight(22)
    APadding = 5
    AOnEdgeValue = 180
    APixelDistScale = 180/5.0 = 36.0

  This will create an SDF bitmap in which the character is about 22 pixels high
  but the whole bitmap is about 22+5+5=32 pixels high. To produce a filled
  shape, sample the SDF at each pixel and fill the pixel if the SDF value is
  greater than or equal to 180/255. (You'll actually want to antialias, which is
  beyond the scope of this example.) Additionally, you can compute offset
  outlines (e.g. to stroke the character border inside & outside, or only
  outside). For example, to fill outside the character up to 3 SDF pixels, you
  would compare against (180-36.0*3)/255 = 72/255. The above choice of variables
  maps a range from 5 pixels outside the shape to 2 pixels inside the shape to
  0..255; this is intended primarily for apply outside effects only (the
  interior range is needed to allow proper antialiasing of the font at *smaller*
  sizes)

  The function computes the SDF analytically at each SDF pixel, not by e.g.
  building a higher-res bitmap and approximating it. In theory the quality
  should be as high as possible for an SDF of this size & representation, but
  unclear if this is true in practice (perhaps building a higher-res bitmap and
  computing from that can allow drop-out prevention).

  The algorithm has not been optimized at all, so expect it to be slow if
  computing lots of characters or very large sizes. }
function stbtt_GetGlyphSDF(var AInfo: TStbttFontInfo; const AScale: Single;
  const AGlyph, APadding: Integer; const AOnEdgeValue: Byte;
  const APixelDistScale: Single; out AWidth, AHeight, AXOff, AYOff: Integer): Pointer; cdecl;
  external STB_LIB name _PU + 'stbtt_GetGlyphSDF';

function stbtt_GetCodepointSDF(var AInfo: TStbttFontInfo; const AScale: Single;
  const ACodepoint, APadding: Integer; const AOnEdgeValue: Byte;
  const APixelDistScale: Single; out AWidth, AHeight, AXOff, AYOff: Integer): Pointer; cdecl;
  external STB_LIB name _PU + 'stbtt_GetCodepointSDF';

{$MINENUMSIZE 4}
type
  { Font styles }
  TStbttFontStyle = (
    { Bold }
    Bold = 0,

    { Italic }
    Italic = 1,

    { Underline }
    Underline = 2,

    { None }
    None = 3);
  TStbttFontStyles = set of TStbttFontStyle;

type
  { TrueType platforms }
  TStbttPlatform = (
    { Unicode }
    Unicode = 0,

    { Mac }
    Mac = 1,

    { ISO }
    ISO = 2,

    { Microsoft }
    Microsoft = 3);

const
  { EncodingID's for Unicode platform. }
  STBTT_UNICODE_EID_UNICODE_1_0      = 0;
  STBTT_UNICODE_EID_UNICODE_1_1      = 1;
  STBTT_UNICODE_EID_ISO_10646        = 2;
  STBTT_UNICODE_EID_UNICODE_2_0_BMP  = 3;
  STBTT_UNICODE_EID_UNICODE_2_0_FULL = 4;

const
  { EncodingID's for Microsoft platform. }
  STBTT_MS_EID_SYMBOL       = 0;
  STBTT_MS_EID_UNICODE_BMP  = 1;
  STBTT_MS_EID_SHIFTJIS     = 2;
  STBTT_MS_EID_UNICODE_FULL = 10;

const
  { EncodingID's for Mac platform. Same as Script Manager codes. }
  STBTT_MAC_EID_ROMAN        = 0;
  STBTT_MAC_EID_JAPANESE     = 1;
  STBTT_MAC_EID_CHINESE_TRAD = 2;
  STBTT_MAC_EID_KOREAN       = 3;
  STBTT_MAC_EID_ARABIC       = 4;
  STBTT_MAC_EID_HEBREW       = 5;
  STBTT_MAC_EID_GREEK        = 6;
  STBTT_MAC_EID_RUSSIAN      = 7;

const
  { LanguageID's for Microsoft platform. Same as LCID.
    Problematic because there are e.g. 16 english LCIDs and 16 arabic LCIDs }
  STBTT_MS_LANG_GERMAN      = $0407;
  STBTT_MS_LANG_ENGLISH     = $0409;
  STBTT_MS_LANG_SPANISH     = $0409;
  STBTT_MS_LANG_FRENCH      = $040c;
  STBTT_MS_LANG_HEBREW      = $040d;
  STBTT_MS_LANG_ITALIAN     = $0410;
  STBTT_MS_LANG_JAPANESE    = $0411;
  STBTT_MS_LANG_KOREAN      = $0412;
  STBTT_MS_LANG_DUTCH       = $0413;
  STBTT_MS_LANG_RUSSIAN     = $0419;
  STBTT_MS_LANG_SWEDISH     = $041D;
  STBTT_MS_LANG_CHINESE     = $0804;

const
  { LanguageID's for Mac platform }
  STBTT_MAC_LANG_ENGLISH            = 0;
  STBTT_MAC_LANG_FRENCH             = 1;
  STBTT_MAC_LANG_GERMAN             = 2;
  STBTT_MAC_LANG_ITALIAN            = 3;
  STBTT_MAC_LANG_DUTCH              = 4;
  STBTT_MAC_LANG_SWEDISH            = 5;
  STBTT_MAC_LANG_SPANISH            = 6;
  STBTT_MAC_LANG_HEBREW             = 10;
  STBTT_MAC_LANG_JAPANESE           = 11;
  STBTT_MAC_LANG_ARABIC             = 12;
  STBTT_MAC_LANG_CHINESE_TRAD       = 19;
  STBTT_MAC_LANG_KOREAN             = 23;
  STBTT_MAC_LANG_RUSSIAN            = 32;
  STBTT_MAC_LANG_CHINESE_SIMPLIFIED = 33;

{ Finding the right font...

  You should really just solve this offline, keep your own tables of what font
  is what, and don't try to get it out of the .ttf file. That's because getting
  it out of the .ttf file is really hard, because the names in the file can
  appear in many possible encodings, in many possible languages, and e.g. if you
  need a case-insensitive comparison, the details of that depend on the encoding
  & language in a complex way (actually underspecified in truetype, but also
  gigantic).

  Parameters:
    ATtfData: pointer to the TTF data.
    AName: name of the font to find.
    AStyles: the font style(s) to search for.

  Returns:
    The offset (not index) of the font that matches, or -1 if not found.

  If AStyles is an empty set, then you must specify the exact font name with
  style designator (like 'Arial Bold'). Otherwise, use a font name without style
  designator (like 'Arial').

  This function will use @bold(case-sensitive) comparisons on unicode-encoded
  names to try to find the font you want. You can run this before calling
  stbtt_InitFont. }
function stbtt_FindMatchingFont(const ATtfData: Pointer;
  const AName: MarshaledAString; const AStyles: TStbttFontStyles): Integer; cdecl;
  external STB_LIB name _PU + 'stbtt_FindMatchingFont';

{ Lets you get any of the various strings from the TTF file yourself and do your
  own comparisons on them (possibly using stbtt_CompareUTF8toUTF16_bigendian).

  Parameters:
    AInfo: font information (acquired with stbtt_InitFont).
    ALength: is set to the length of the string.
    APlatform: the platform to use.
    AEncodingID: the encoding to use.
    ALanguageID: the language to use.
    ANameID: the name to use.

  Returns:
    The string (which may be big-endian double byte, e.g. for unicode). The
    length of the string is put in ALength.

  Some of the values for the IDs are defined as STBTT_* constants. For more see
  the truetype spec:
  * http://developer.apple.com/textfonts/TTRefMan/RM06/Chap6name.html
  * http://www.microsoft.com/typography/otspec/name.htm}
function stbtt_GetFontNameString(var AInfo: TStbttFontInfo; out ALength: Integer;
  const APlatform: TStbttPlatform; const AEncodingID, ALanguageID, ANameID: Integer): Pointer; cdecl;
  external STB_LIB name _PU + 'stbtt_GetFontNameString';

{ Can be used to compare string returned by stbtt_GetFontNameString.

  Parameters:
    AUtf8String: pointer to a UTF-8 string.
    AUtf8Length: length of AUtf8String.
    AUtf16String: pointer to a big-endian UTF-16 string.
    AUtf16Length: length of AUtf16String.

  Returns:
    True if AUtf8String equals AUtf16String. False otherwise. }
function stbtt_CompareUTF8toUTF16_bigendian(const AUtf8String: MarshaledAString;
  const AUtf8Length: Integer; const AUtf16String: MarshaledString;
  const AUtf16Length: Integer): LongBool; cdecl;
  external STB_LIB name _PU + 'stbtt_CompareUTF8toUTF16_bigendian';

implementation

function StbttPointSize(const AValue: Single): Single; inline;
begin
  Result := -AValue;
end;

end.
