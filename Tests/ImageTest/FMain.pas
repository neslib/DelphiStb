unit FMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Layouts;

type
  TFormMain = class(TForm)
    Memo: TMemo;
    Layout1: TLayout;
    Image: TImage;
    ButtonRunTests: TButton;
    procedure ButtonRunTestsClick(Sender: TObject);
  private
    { Private declarations }
    procedure Log(const AMsg: String); overload;
    procedure Log(const AMsg: String; const AArgs: array of const); overload;
    procedure CheckImage(const ATest, AFileType: String; const AImage: Pointer;
      const AWidth, AHeight, AComponents, AReqComp: Integer);
    procedure TestLoadFromMemory(const AFileType: String);
    procedure TestLoadFromCallbacks(const AFileType: String);
    procedure TestWrite;
    procedure ShowImage(const AData: Pointer; const AWidth, AHeight: Integer);
  private
    class function CallbackRead(const AUserData, AData: Pointer;
      const ASize: Integer): Integer; cdecl; static;
    class procedure CallbackSkip(const AUserData: Pointer;
      const ASize: Integer); cdecl; static;
    class function CallbackEof(const AUserData: Pointer): LongBool; cdecl; static;
    class procedure WriteFunc(const AUserData, AData: Pointer;
      const ASize: Integer); cdecl; static;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

const
  FILE_TYPES: array [0..6] of String = (
    'BMP', 'GIF', 'JPG', 'PBM', 'PNG', 'PSD', 'TGA');

implementation

{$R *.fmx}

uses
  Neslib.Stb.Image,
  Neslib.Stb.ImageWrite;

procedure TFormMain.ButtonRunTestsClick(Sender: TObject);
var
  FileType: String;
begin
  Memo.Lines.Clear;
  for FileType in FILE_TYPES do
  begin
    TestLoadFromMemory(FileType);
    TestLoadFromCallbacks(FileType);
  end;
  TestWrite;
  Memo.Lines.Add('Tests completed');
end;

class function TFormMain.CallbackEof(const AUserData: Pointer): LongBool;
var
  Stream: TStream;
begin
  Stream := AUserData;
  Result := (Stream.Position >= Stream.Size);
end;

class function TFormMain.CallbackRead(const AUserData, AData: Pointer;
  const ASize: Integer): Integer;
var
  Stream: TStream;
begin
  Stream := AUserData;
  Result := Stream.Read(AData^, ASize);
end;

class procedure TFormMain.CallbackSkip(const AUserData: Pointer;
  const ASize: Integer);
var
  Stream: TStream;
begin
  Stream := AUserData;
  Stream.Seek(Longint(ASize), soFromCurrent);
end;

procedure TFormMain.CheckImage(const ATest, AFileType: String; const AImage: Pointer;
  const AWidth, AHeight, AComponents, AReqComp: Integer);
var
  ExpComp: Integer;
begin
  if (AImage = nil) then
  begin
    Log('%s(%s, %d): cannot load image', [ATest, AFileType, AReqComp]);
    Exit;
  end;

  if (AWidth <> 100) then
    Log('%s(%s): invalid width. Expected 100, was %d',
      [ATest, AFileType, AWidth]);

  if (AHeight <> 70) then
    Log('%s(%s): invalid height. Expected 70, was %d',
      [ATest, AFileType, AHeight]);

  if (AFileType = 'PNG') or (AFileType = 'GIF') or (AFileType = 'PSD') then
    ExpComp := 4
  else
    ExpComp := 3;

  if (AComponents <> ExpComp) then
    Log('%s(%s): invalid components. Expected %d, was %d',
      [ATest, AFileType, ExpComp, AComponents]);
end;

procedure TFormMain.Log(const AMsg: String);
begin
  Memo.Lines.Add(AMsg);
  Application.ProcessMessages;
end;

procedure TFormMain.Log(const AMsg: String; const AArgs: array of const);
begin
  Memo.Lines.Add(Format(AMsg, AArgs));
  Application.ProcessMessages;
end;

procedure TFormMain.ShowImage(const AData: Pointer; const AWidth,
  AHeight: Integer);
var
  Bitmap: TBitmap;
  BitmapData: TBitmapData;
  X, Y: Integer;
  S, D: PByte;
begin
  Bitmap := TBitmap.Create(AWidth, AHeight);
  try
    if (Bitmap.Map(TMapAccess.Write, BitmapData)) then
    try
      S := AData;
      D := BitmapData.Data;
      for Y := 0 to AHeight - 1 do
      begin
        for X := 0 to AWidth - 1 do
        begin
          {$IFDEF MSWINDOWS}
          D[0] := S[2];
          D[1] := S[1];
          D[2] := S[0];
          D[3] := S[3];
          {$ELSE}
          D[0] := S[0];
          D[1] := S[1];
          D[2] := S[2];
          D[3] := S[3];
          {$ENDIF}
          Inc(D, 4);
          Inc(S, 4);
        end;
        Inc(D, BitmapData.Pitch - (AWidth * 4));
      end;
    finally
      Bitmap.Unmap(BitmapData);
    end;
    Image.Bitmap := Bitmap;
  finally
    Bitmap.Free;
  end;
end;

procedure TFormMain.TestLoadFromCallbacks(const AFileType: String);
var
  Stream: TStream;
  Callbacks: TStbiIOCallbacks;
  Image: Pointer;
  Width, Height, Components, ReqComp: Integer;
begin
  Callbacks.Read := CallbackRead;
  Callbacks.Skip := CallbackSkip;
  Callbacks.Eof := CallbackEof;

  Stream := TResourceStream.Create(HInstance, AFileType, RT_RCDATA);
  try
    for ReqComp := 0 to 4 do
    begin
      Image := stbi_load_from_callbacks(Callbacks, Stream, Width, Height,
        Components, ReqComp);
      try
        CheckImage('TestLoadFromCallbacks', AFileType, Image, Width, Height,
          Components, ReqComp);
      finally
        stbi_image_free(Image);
      end;

      Stream.Position := 0;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TFormMain.TestLoadFromMemory(const AFileType: String);
var
  Stream: TStream;
  Data: TBytes;
  Image: Pointer;
  Width, Height, Components, ReqComp: Integer;
begin
  Stream := TResourceStream.Create(HInstance, AFileType, RT_RCDATA);
  try
    SetLength(Data, Stream.Size);
    Stream.ReadBuffer(Data[0], Stream.Size);
  finally
    Stream.Free;
  end;

  for ReqComp := 0 to 4 do
  begin
    Image := stbi_load_from_memory(@Data[0], Length(Data), Width, Height,
      Components, ReqComp);
    try
      CheckImage('TestLoadFromMemory', AFileType, Image, Width, Height,
        Components, ReqComp);
    finally
      stbi_image_free(Image);
    end;
  end;
end;

procedure TFormMain.TestWrite;
var
  Stream: TStream;
  Data: TBytes;
  Image, Recon: Pointer;
  Width, Height, Components: Integer;
  ReconWidth, ReconHeight, ReconComponents: Integer;
  Callbacks: TStbiIOCallbacks;
begin
  Stream := TResourceStream.Create(HInstance, 'PSD', RT_RCDATA);
  try
    SetLength(Data, Stream.Size);
    Stream.ReadBuffer(Data[0], Stream.Size);
  finally
    Stream.Free;
  end;

  Image := stbi_load_from_memory(@Data[0], Length(Data), Width, Height, Components);
  if (Image = nil) then
  begin
    Log('Error loading PSD image');
    Exit;
  end;

  ShowImage(Image, Width, Height);

  Stream := nil;
  try
    Stream := TMemoryStream.Create;
    Callbacks.Read := CallbackRead;
    Callbacks.Skip := CallbackSkip;
    Callbacks.Eof := CallbackEof;

    { PNG }
    stbi_write_png_to_func(WriteFunc, Stream, Width, Height, Components, Image, Width * 4);
    Stream.Position := 0;
    Recon := stbi_load_from_callbacks(Callbacks, Stream, ReconWidth, ReconHeight, ReconComponents, 0);
    CheckImage('TestWrite', 'PNG', Recon, ReconWidth, ReconHeight, ReconComponents, 0);

    { BMP }
    Stream.Position := 0;
    stbi_write_bmp_to_func(WriteFunc, Stream, Width, Height, Components, Image);
    Stream.Position := 0;
    Recon := stbi_load_from_callbacks(Callbacks, Stream, ReconWidth, ReconHeight, ReconComponents, 0);
    CheckImage('TestWrite', 'BMP', Recon, ReconWidth, ReconHeight, ReconComponents, 0);

    { TGA }
    Stream.Position := 0;
    stbi_write_tga_to_func(WriteFunc, Stream, Width, Height, Components, Image);
    Stream.Position := 0;
    Recon := stbi_load_from_callbacks(Callbacks, Stream, ReconWidth, ReconHeight, ReconComponents, 0);
    ReconComponents := 3; { TGA actually outputs 4 bytes per pixel, but the test function expects 3 }
    CheckImage('TestWrite', 'TGA', Recon, ReconWidth, ReconHeight, ReconComponents, 0);
  finally
    Stream.Free;
    stbi_image_free(Image);
  end;
end;

class procedure TFormMain.WriteFunc(const AUserData, AData: Pointer;
  const ASize: Integer);
var
  Stream: TStream;
begin
  Stream := AUserData;
  Stream.Write(AData^, ASize);
end;

end.
