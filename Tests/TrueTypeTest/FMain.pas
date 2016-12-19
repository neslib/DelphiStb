unit FMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TFormMain = class(TForm)
    LabelAtlas: TLabel;
    ImageAtlas: TImage;
    LabelSample: TLabel;
    ImageSample: TImage;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  Neslib.Stb.TrueType;

procedure TFormMain.FormCreate(Sender: TObject);
const
  ATLAS_WIDTH = 320;
  ATLAS_HEIGHT = 320;
  SAMPLE_WIDTH = ATLAS_WIDTH;
  SAMPLE_HEIGHT = 50;
var
  TtfData, Atlas: TBytes;
  Stream: TStream;
  PackContext: TStbttPackContext;
  PackedChars: TArray<TStbttPackedChar>;
  BitmapAtlas, BitmapSample: TBitmap;
  BitmapData: TBitmapData;
  S, D: PByte;
  Col, Row: Integer;
  X, Y: Single;
  SR, DR: TRectF;
  PC: TStbttPackedChar;
  C: Char;
  Q: TStbttAlignedQuad;
begin
  { Load TTF font }
  Stream := TResourceStream.Create(HInstance, 'DROID_SANS', RT_RCDATA);
  try
    SetLength(TtfData, Stream.Size);
    Stream.ReadBuffer(TtfData[0], Stream.Size);
  finally
    Stream.Free;
  end;

  { Create font atlas }
  SetLength(Atlas, ATLAS_WIDTH * ATLAS_HEIGHT);
  SetLength(PackedChars, 256);
  stbtt_PackBegin(PackContext, Atlas, ATLAS_WIDTH, ATLAS_HEIGHT, ATLAS_WIDTH, 1);
  try
    stbtt_PackFontRange(PackContext, TtfData, 0, 32, 0, 256, @PackedChars[0]);
  finally
    stbtt_PackEnd(PackContext);
  end;

  { Convert atlas to FireMonkey BitmapAtlas }
  BitmapSample := nil;
  BitmapAtlas := TBitmap.Create(ATLAS_WIDTH, ATLAS_HEIGHT);
  try
    if (BitmapAtlas.Map(TMapAccess.Write, BitmapData)) then
    try
      S := @Atlas[0];
      D := BitmapData.Data;
      for Row := 0 to ATLAS_HEIGHT - 1 do
      begin
        for Col := 0 to ATLAS_WIDTH - 1 do
        begin
          D[0] := S[0];
          D[1] := S[0];
          D[2] := S[0];
          D[3] := $FF;
          Inc(D, 4);
          Inc(S);
        end;
        Inc(D, BitmapData.Pitch - (ATLAS_WIDTH * 4));
      end;
    finally
      BitmapAtlas.Unmap(BitmapData);
    end;

    { Highlight "A" in atlas }
    PC := PackedChars[Ord('A')];
    BitmapAtlas.Canvas.BeginScene;
    try
      BitmapAtlas.Canvas.Stroke.Color := TAlphaColors.Red;
      BitmapAtlas.Canvas.DrawRect(RectF(PC.Left - 0.5, PC.Top - 0.5,
        PC.Right + 0.5, PC.Bottom + 0.5), 0, 0, [], 1);
    finally
      BitmapAtlas.Canvas.EndScene;
    end;
    ImageAtlas.Bitmap := BitmapAtlas;

    { Draw sample text from atlas }
    BitmapSample := TBitmap.Create(SAMPLE_WIDTH, SAMPLE_HEIGHT);
    BitmapSample.Canvas.BeginScene;
    try
      BitmapSample.Canvas.Fill.Color := TAlphaColors.Black;
      BitmapSample.Canvas.FillRect(RectF(0, 0, SAMPLE_WIDTH, SAMPLE_HEIGHT), 0, 0, [], 1);

      X := 0;
      Y := SAMPLE_HEIGHT div 2;
      for C in 'stb_truetype' do
      begin
        stbtt_GetPackedQuad(@PackedChars[0], ATLAS_WIDTH, ATLAS_HEIGHT, Ord(C), X, Y, Q, False);

        DR.Left := Q.X0;
        DR.Top := Q.Y0;
        DR.Right := Q.X1;
        DR.Bottom := Q.Y1;

        { TStbttAlignedQuad contains texture coordinates for hardware blitting.
          We need to convert these to regular bitmap coordinates for FireMonkey. }
        SR.Left := Round(Q.S0 * ATLAS_WIDTH);
        SR.Top := Round(Q.T0 * ATLAS_HEIGHT);
        SR.Width := DR.Width;
        SR.Height := DR.Height;

        BitmapSample.Canvas.DrawBitmap(BitmapAtlas, SR, DR, 1);
      end;
    finally
      BitmapSample.Canvas.EndScene;
    end;
    ImageSample.Bitmap := BitmapSample;
  finally
    BitmapSample.Free;
    BitmapAtlas.Free;
  end;
end;

end.
