unit Neslib.Stb.RectPack;
{< stb_rect_pack: simple 2D rectangle packer with decent quality.

  Useful for e.g. packing rectangular textures into an atlas.
  Does not do rotation.

  Not necessarily the awesomest packing method, but better than the totally
  naive one in stb_truetype (which is primarily what this is meant to replace).

  Has only had a few tests run, may have issues.

  This library currently uses the Skyline Bottom-Left and Best-Fit algorithms. }

{$MINENUMSIZE 4}

interface

uses
  Neslib.Stb.Common;

type
  { Type for coordinates in a packed rectangle }
  TStbrpCoord = UInt16;

type
  { Information about a packed rectangle }
  TStbrpRect = record
    { Reserved for your use }
    Id: Integer;

    { Dimensions }
    W, H: TStbrpCoord;

    { Position }
    X, Y: TStbrpCoord;

    { Whether this rect was packed }
    WasPacked: LongBool;
  end;
  PStbrpRect = ^TStbrpRect;

type
  { This is an opaque structure that you shouldn't mess with which holds
    data for a rect packing node. }
  TStbrpNode = record
    _Data: array [0..SizeOf(Pointer) + (SizeOf(TStbrpCoord) * 2) - 1] of Byte;
  end;
  PStbrpNode = ^TStbrpNode;

type
  { This is an opaque structure that you shouldn't mess with which holds
    all the context needed for rect packing. }
  TStbrpContext = record
    _Data: array [0..(SizeOf(Pointer) * 2) + (SizeOf(TStbrpNode) * 2) + (SizeOf(Integer) * 6) - 1] of Byte;
  end;
  PStbrpContext = ^TStbrpContext;

{ Initialize a rectangle packer to pack a rectangle that is AWidth by AHeight in
  dimensions using temporary storage provided by the array ANodes.

  Parameters:
    AContext: is fill with a context that you can pass later to other
      stbrp_* routines.
    AWidth: width of the container rectangle.
    AHeight: height of the container rectangle.
    ANodes: array of TStbrpNode used for temporary storage. See remarks.
    ANumNodes: number of nodes in the ANodes array.

  You must call this function every time you start packing into a new target.

  There is no "shutdown" function. The ANodes array memory must stay valid for
  the following stbrp_pack_rects() call (or calls), but can be released after
  the call (or calls) finish.

  Note: to guarantee best results, either:
  * make sure <tt>ANumNodes >= AWidth</tt>
  * or, <tt>stbrp_allow_out_of_mem(True)</tt>

  If you don't do either of the above things, widths will be quantized to
  multiples of small integers to guarantee the algorithm doesn't run out of
  temporary storage.

  If you call <tt>stbrp_allow_out_of_mem(True), then the non-quantized algorithm
  will be used, but the algorithm may run out of temporary storage and be unable
  to pack some rectangles. }
procedure stbrp_init_target(out AContext: TStbrpContext; const AWidth,
  AHeight: Integer; const ANodes: TArray<TStbrpNode>; const ANumNodes: Integer); cdecl;
  external STB_LIB name _PU + 'stbrp_init_target';

{ Optionally call this function after stbrp_init_target but before doing any
  packing to change the handling of the out-of-temp-memory scenario.

  Parameters:
    AContext: the context, as filled in by stbrp_init_target.
    AAllowOutOfMem: whether to allow for out-of-temp-memory conditions.
      See stbrp_init_target for details.;

  If you call init stbrp_init_target, this will be reset to the default (False). }
procedure stbrp_setup_allow_out_of_mem(var AContext: TStbrpContext;
  const AAllowOutOfMem: LongBool); cdecl;
  external STB_LIB name _PU + 'stbrp_setup_allow_out_of_mem';

type
  { Heuristics for use with stbrp_setup_heuristic }
  TStbrpHeuristic = (
    { Default heuristic.
      Currently equals the Skyline Bottom-Left algorithm. }
    SkylineDefault = 0,

    { Skyline Bottom-Left algorithm. }
    SkylineBottomLeft= SkylineDefault,

    { Skyline Best-Fit algorithm. }
    SkylineBestFit);

{ Optionally select which packing heuristic the library should use.

  Parameters:
    AContext: the context, as filled in by stbrp_init_target.
    AHeuristic: the heuristic to use.

  Different heuristics will produce better/worse results for different data
  sets. If you call stbrp_init_target again, this will be reset to the default. }
procedure stbrp_setup_heuristic(var AContext: TStbrpContext;
  const AHeuristic: TStbrpHeuristic); cdecl;
  external STB_LIB name _PU + 'stbrp_setup_heuristic';

{ Assign packed locations to rectangles.

  Parameters:
    AContext: the context, as filled in by stbrp_init_target.
    ARectangles: pointer to an array of rectangles to pack. Both input and
      output. See remarks.
    ANumRectangles: the number of rectangles in the ARectangles array.

  Returns:
    True if all of the rectangles were successfully packed. False otherwise.

  In input, each rectangle in ARectangles must have the following fields set:
  * W: width of the rectangle
  * H: height of the rectangle
  * Id: optionally, you can assign some Id to the rectangle so you can track it
    later.

  After the function returns, the following fields of each rectangle are set:
  * WasPacked: is set to True if the rectangle could be packed into the
    container rectangle. Is set to False if the rectangle is too big, or there
    is no room left in the container rectangle.
  * X: X-position of the rectangle in the container rectangle (0=Left).
  * Y: Y-position of the rectangle in the container rectangle (0=Top).

  You should not try to access the ARectangles array from another thread while
  this function is running, as the function temporarily reorders the array while
  it executes.

  To pack into another rectangle, you need to call stbrp_init_target again. To
  continue packing into the same rectangle, you can call this function again.
  Calling this multiple times with multiple rect arrays will probably produce
  worse packing results than calling it a single time with the full rectangle
  array, but the option is available. }
function stbrp_pack_rects(var AContext: TStbrpContext;
  const ARectangles: PStbrpRect; const ANumRectangles: Integer): LongBool; cdecl;
  external STB_LIB name _PU + 'stbrp_pack_rects';

implementation

end.
