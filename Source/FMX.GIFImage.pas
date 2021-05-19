{ ******************************************************************************
                            GIFImage for Firemonkey
            Copyright (C) 2020 by Thomas Dannert <thomas@dannert.com>
                      https://github.com/TomDannert/GifImage
  ******************************************************************************
  GIFImage for Firemonkey is free software: you can redistribute it and / or
  modify it under the terms of the GNU Lesser General Public License version
  published by the Free Software Foundation and appearing in the included file.
  GIFImage for Firemonkey is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
  for more details.
  ****************************************************************************** }

unit FMX.GIFImage;

interface

uses
  System.Types,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.UITypes,
  System.Messaging,
  FMX.Types,
  FMX.Controls,
  FMX.Objects,
  FMX.Graphics;

type
  TAlphaColorRecArray = array[0..MaxInt div SizeOf(TAlphaColorRec) - 1] of TAlphaColorRec;
  PAlphaColorRecArray = ^TAlphaColorRecArray;
  TPalette32 = TAlphaColorRecArray;
  PPalette32 = ^TPalette32;
  TPalette32Size256 = array[0..255] of TAlphaColorRec;
  TTripleChar = array[0..2] of Byte;

  { ************************************************************************** }
  { TGIFPaletteImage }
  { ************************************************************************** }

  TGIFPaletteImage = packed record
    Width   : Integer;
    Height  : Integer;
    Delay   : Integer;
    Size    : Integer;
    Bits    : Pointer;
    Palette : PPalette32;
    Tag     : Pointer;
  end;
  PGIFImageData = ^TGIFPaletteImage;

  { ************************************************************************** }
  { TGIFData }
  { ************************************************************************** }

  TGIFDisposalMethod = (dmNoRemoval, dmLeave, dmRestoreBackground, dmRestorePrevious, dmReserved4, dmReserved5, dmReserved6, dmReserved7);

  TGIFData = class(TPersistent)
  private type

    TGIFHeader = packed record
      Signature      : TTripleChar;
      Version        : TTripleChar;
      ScreenWidth    : Word;
      ScreenHeight   : Word;
      PackedFields   : Byte;
      BackColorIndex : Byte;
      AspectRatio    : Byte;
    end;

    TGIFFrame = class(TObject)
    private
      FHasLocalPal     : Boolean;
      FTransIndex      : Integer;
      FBackIndex       : Integer;
      FLeft            : Integer;
      FTop             : Integer;
      FWidth           : Integer;
      FHeight          : Integer;
      FDisposal        : TGIFDisposalMethod;
      FDelay           : Integer;
      FHasTransparency : Boolean;
      FData            : TGIFPaletteImage;
    protected
      property Data : TGIFPaletteImage read FData;
      property TransIndex : Integer read FTransIndex;
      property HasLocalPal : Boolean read FHasLocalPal;
      property BackIndex : Integer read FBackIndex;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      property Left : Integer read FLeft;
      property Top : Integer read FTop;
      property Width : Integer read FWidth;
      property Height : Integer read FHeight;
      property Disposal : TGIFDisposalMethod read FDisposal;
      property Delay : Integer read FDelay;
      property HasTransparency : Boolean read FHasTransparency;
    end;

    TGIFFrames = TObjectList<TGIFFrame>;

  private
    FStream        : TMemoryStream;
    FHeader        : TGIFHeader;
    FFrames        : TGIFFrames;
    FCachedFrame   : TBitmap;
    FCachedIndex   : Integer;
    FHasPalette    : Boolean;
    FPaletteLength : Integer;
    FPalette       : TPalette32Size256;
    function GetFrame(const AIndex : Integer) : TGIFFrame;
    function GetFrameCount : Integer;
    procedure InternalClear;
    procedure InternalRender(AIndex: Integer; const ABitmap: TBitmap; const ACached : TBitmap; var ACachedIndex : Integer);
  protected
    procedure DoChange; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    property Stream : TMemoryStream read FStream;
    property HasPalette : Boolean read FHasPalette;
    property Palette : TPalette32Size256 read FPalette;
    property PaletteLength : Integer read FPaletteLength;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
    function EqualsTo(AData : TGIFData) : Boolean;
    function IsEmpty : Boolean;
    procedure LoadFromStream(AStream : TStream);
    procedure SaveToStream(AStream : TStream);
    procedure LoadFromFile(const AFilename : String);
    procedure SaveToFile(const AFilename : String);
    procedure Render(AFrameIndex : Integer; ABitmap : TBitmap);
    procedure Clear;
    property Header : TGIFHeader read FHeader;
    property Frames[const Index : Integer] : TGIFFrame read GetFrame;
    property FrameCount : Integer read GetFrameCount;
  end;

  { ************************************************************************** }
  { TGIFImage }
  { ************************************************************************** }

  TGIFImage = class(TControl)
  private
    FDisableInterpolation : Boolean;
    FScaleChangedId       : Integer;
    FAutoPlay             : Boolean;
    FWrapMode             : TImageWrapMode;
    FSpeed                : Single;
    FScreenScale          : Single;
    FBitmap               : TBitmap;
    FFill                 : TBrush;
    FGIFData              : TGIFData;
    FPlayer               : TTimer;
    FLoop                 : Boolean;
    FActiveFrame          : Integer;
    FOnPlay               : TNotifyEvent;
    FOnStop               : TNotifyEvent;
    FOnChange             : TNotifyEvent;
    procedure SetBitmap(const AValue: TBitmap);
    procedure SetGIFData(const AValue : TGIFData);
    procedure SetWrapMode(const AValue: TImageWrapMode);
    procedure SetIsPlaying(const AValue : Boolean);
    procedure SetActiveFrame(const AValue : Integer);
    procedure SetSpeed(const AValue : Single);
    procedure SetFill(const AValue : TBrush);
    function GetIsPlaying : Boolean;
    function GetIsEmpty : Boolean;
    procedure ScaleChangedHandler(const Sender: TObject; const Msg: TMessage);
    procedure DrawBitmap(const Canvas: TCanvas; const ARect: TRectF; const ABitmap: TBitmap; const AOpacity: Single = 1.0);
    procedure DoRenderNextFrame(Sender : TObject);
    procedure DoFillChanged(Sender : TObject);
  protected
    procedure DoImageChanged; virtual;
    procedure Paint; override;
    procedure Loaded; override;
    procedure DoPlay; virtual;
    procedure DoStop; virtual;
    procedure DoChange; virtual;
    procedure VisibleChanged; override;
    function IsSpeedStored : Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Play;
    procedure Stop;
    procedure LoadFromStream(AStream : TStream);
    procedure SaveToStream(AStream : TStream);
    procedure LoadFromFile(const AFilename : String);
    procedure SaveToFile(const AFilename : String);
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property IsEmpty : Boolean read GetIsEmpty;
  published
    property AutoPlay : Boolean read FAutoPlay write FAutoPlay default True;
    property ActiveFrame : Integer read FActiveFrame write SetActiveFrame default 0;
    property DisableInterpolation: Boolean read FDisableInterpolation write FDisableInterpolation default False;
    property Fill : TBrush read FFill write SetFill;
    property GIFData : TGIFData read FGIFData write SetGIFData;
    property IsPlaying : Boolean read GetIsPlaying write SetIsPlaying stored False;
    property Loop : Boolean read FLoop write FLoop default True;
    property Speed : Single read FSpeed write SetSpeed stored IsSpeedStored;
    property WrapMode: TImageWrapMode read FWrapMode write SetWrapMode default TImageWrapMode.Fit;
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property Hint;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property Visible default True;
    property Width;
    property ParentShowHint;
    property ShowHint;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property OnPlay : TNotifyEvent read FOnPlay write FOnPlay;
    property OnStop : TNotifyEvent read FOnStop write FOnStop;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
  end;

implementation

uses
  System.Math,
  System.Diagnostics,
  FMX.Forms;

const
  GIF_GlobalColorTable           = $80;
  GIF_ColorTableSize             = $07;
  GIF_LocalColorTable            = $80;
  GIF_Interlaced                 = $40;
  GIF_PlainText: Byte            = $01;
  GIF_GraphicExtension: Byte     = $F9;
  GIF_CommentExtension: Byte     = $FE;
  GIF_ApplicationExtension: Byte = $FF;
  GIF_ImageDescriptor: Byte      = Ord(',');
  GIF_ExtensionIntroducer: Byte  = Ord('!');
  GIF_Trailer: Byte              = Ord(';');
  GIF_DefaultDelay               = 65;
  GIF_Transparent                = $01;
  GIF_DisposalMethod             = $1C;
  GIF_AppLoopExtension           = 1;
  GIF_AppBufferExtension         = 2;

type
  TCardinalArray = array[0..MaxInt div 8 - 1] of Cardinal;
  PCardinalArray = ^TCardinalArray;

  TCardinalRec = packed record
  case Integer of
    0: (CardinalValue: Cardinal);
    1: (Low, High: Word);
    2: (Words: array[0..1] of Word);
    3: (Bytes: array[0..3] of Byte);
  end;
  PCardinalRec = ^TCardinalRec;
  TCardinalRecArray = array[0..MaxInt div 8 - 1] of TCardinalRec;
  PCardinalRecArray = ^TCardinalRecArray;

  TGIFImageDescriptor = packed record
    Left: Word;
    Top: Word;
    Width: Word;
    Height: Word;
    PackedFields: Byte;
  end;

const
  CodeTableSize = 4096;
  HashTableSize = 17777;

type
  TGIFGraphicControlExtension = packed record
    BlockSize: Byte;
    PackedFields: Byte;
    DelayTime: Word;
    TransparentColorIndex: Byte;
    Terminator: Byte;
  end;

  TGIFIdentifierCode = array[0..7] of Byte;
  TGIFAuthenticationCode = array[0..2] of Byte;
  TGIFApplicationRec = packed record
    Identifier: TGIFIdentifierCode;
    Authentication: TGIFAuthenticationCode;
  end;

  TGIFReadContext = record
    Inx: Integer;
    Size: Integer;
    Buf: array [0..255 + 4] of Byte;
    CodeSize: Integer;
    ReadMask: Integer;
  end;
  PGIFReadContext = ^TGIFReadContext;

  TGIFOutputContext = record
    W: Integer;
    H: Integer;
    X: Integer;
    Y: Integer;
    BitsPerPixel: Integer;
    Pass: Integer;
    Interlace: Boolean;
    LineIdent: Integer;
    Data: Pointer;
    CurrLineData: Pointer;
  end;

  PIntCodeTable = ^TIntCodeTable;
  TIntCodeTable = array [0..CodeTableSize - 1] of Word;

  EGIFImageError = class(Exception);

procedure ClipRectBounds(var X, Y, Width, Height: Integer; const Clip: TRect);

  procedure ClipDim(var AStart, ALength: Integer; ClipMin, ClipMax: Integer);
  begin
  if AStart < ClipMin then
    begin
      ALength := ALength - (ClipMin - AStart);
      AStart := ClipMin;
    end;
    if AStart + ALength > ClipMax then ALength := Max(0, ClipMax - AStart);
  end;

begin
  ClipDim(X, Width, Clip.Left, Clip.Right);
  ClipDim(Y, Height, Clip.Top, Clip.Bottom);
end;

procedure LZWDecompress(Stream: TStream; Width, Height: Integer; Interlaced: Boolean; Data: Pointer);
var
  MinCodeSize: Byte;
  MaxCode, BitMask, InitCodeSize: Integer;
  ClearCode, EndingCode, FirstFreeCode, FreeCode: Word;
  I, OutCount, Code: Integer;
  CurCode, OldCode, InCode, FinalChar: Word;
  Prefix, Suffix, OutCode: PIntCodeTable;
  ReadCtxt: TGIFReadContext;
  OutCtxt: TGIFOutputContext;
  TableFull: Boolean;

  function ReadCode(var Context: TGIFReadContext): Integer;
  var
    RawCode: Integer;
    ByteIndex: Integer;
    Bytes: Byte;
    BytesToLose: Integer;
  begin
    while (Context.Inx + Context.CodeSize > Context.Size) and
      (Stream.Position < Stream.Size) do
    begin
      // Not enough bits in buffer - refill it - Not very efficient, but infrequently called
      BytesToLose := Context.Inx shr 3;
      // Note biggest Code Size is 12 bits. And this can at worst span 3 Bytes
      Move(Context.Buf[Word(BytesToLose)], Context.Buf[0], 3);
      Context.Inx := Context.Inx and 7;
      Context.Size := Context.Size - (BytesToLose shl 3);
      Stream.Read(Bytes, 1);
      if Bytes > 0 then
        Stream.Read(Context.Buf[Word(Context.Size shr 3)], Bytes);
      Context.Size := Context.Size + (Bytes shl 3);
    end;
    ByteIndex := Context.Inx shr 3;
    RawCode := Context.Buf[Word(ByteIndex)] +
      (Word(Context.Buf[Word(ByteIndex + 1)]) shl 8);
    if Context.CodeSize > 8 then
      RawCode := RawCode + (Integer(Context.Buf[ByteIndex + 2]) shl 16);
    RawCode := RawCode shr (Context.Inx and 7);
    Context.Inx := Context.Inx + Byte(Context.CodeSize);
    Result := RawCode and Context.ReadMask;
  end;

  function InterlaceStep(Y, Height: Integer; var Pass: Integer): Integer;
  begin
    Result := Y;
    case Pass of
      0, 1: Inc(Result, 8);
      2:    Inc(Result, 4);
      3:    Inc(Result, 2);
    end;
    if Result >= Height then
    begin
      if Pass = 0 then
      begin
        Pass := 1;
        Result := 4;
        if Result < Height then Exit;
      end;
      if Pass = 1 then
      begin
        Pass := 2;
        Result := 2;
        if Result < Height then Exit;
      end;
      if Pass = 2 then
      begin
        Pass := 3;
        Result := 1;
      end;
    end;
  end;

  procedure Output(Value: Byte; var Context: TGIFOutputContext);
  var
    P: PByte;
  begin
    if Context.Y >= Context.H then Exit;
    P := @PByteArray(Context.CurrLineData)[Context.X];
    P^ := Value;
    Inc(Context.X);
    if Context.X < Context.W then Exit;
    Context.X := 0;
    if Context.Interlace then
      Context.Y := InterlaceStep(Context.Y, Context.H, Context.Pass)
    else
      Inc(Context.Y);
    Context.CurrLineData := @PByteArray(Context.Data)[Context.Y * Context.LineIdent];
  end;

begin
  OutCount := 0;
  OldCode := 0;
  FinalChar := 0;
  TableFull := False;
  GetMem(Prefix, SizeOf(TIntCodeTable));
  GetMem(Suffix, SizeOf(TIntCodeTable));
  GetMem(OutCode, SizeOf(TIntCodeTable) + SizeOf(Word));
  try
    Stream.Read(MinCodeSize, 1);
    if (MinCodeSize < 2) or (MinCodeSize > 9) then
    begin
      raise EGIFImageError.Create('Error when decoding GIF data');
    end;
    // Initial read context
    ReadCtxt.Inx := 0;
    ReadCtxt.Size := 0;
    ReadCtxt.CodeSize := MinCodeSize + 1;
    ReadCtxt.ReadMask := (1 shl ReadCtxt.CodeSize) - 1;
    // Initialise pixel-output context
    OutCtxt.X := 0;
    OutCtxt.Y := 0;
    OutCtxt.Pass := 0;
    OutCtxt.W := Width;
    OutCtxt.H := Height;
    OutCtxt.BitsPerPixel := MinCodeSize;
    OutCtxt.Interlace := Interlaced;
    OutCtxt.LineIdent := Width;
    OutCtxt.Data := Data;
    OutCtxt.CurrLineData := Data;
    BitMask := (1 shl OutCtxt.BitsPerPixel) - 1;
    // 2 ^ MinCodeSize accounts for all colours in file
    ClearCode := 1 shl MinCodeSize;
    EndingCode := ClearCode + 1;
    FreeCode := ClearCode + 2;
    FirstFreeCode := FreeCode;
    // 2^ (MinCodeSize + 1) includes clear and eoi Code and space too
    InitCodeSize := ReadCtxt.CodeSize;
    MaxCode := 1 shl ReadCtxt.CodeSize;
    Code := ReadCode(ReadCtxt);
    while (Code <> EndingCode) and (Code <> $FFFF) and (OutCtxt.Y < OutCtxt.H) do
    begin
      if Code = ClearCode then
      begin
        ReadCtxt.CodeSize := InitCodeSize;
        MaxCode := 1 shl ReadCtxt.CodeSize;
        ReadCtxt.ReadMask := MaxCode - 1;
        FreeCode := FirstFreeCode;
        Code := ReadCode(ReadCtxt);
        CurCode := Code;
        OldCode := Code;
        if Code = $FFFF then Break;
        FinalChar := (CurCode and BitMask);
        Output(Byte(FinalChar), OutCtxt);
        TableFull := False;
      end
      else
      begin
        CurCode := Code;
        InCode := Code;
        if CurCode >= FreeCode then
        begin
          CurCode := OldCode;
          OutCode^[OutCount] := FinalChar;
          Inc(OutCount);
        end;
        while CurCode > BitMask do
        begin
          if OutCount > CodeTableSize then
          begin
            raise EGIFImageError.Create('Error when decoding GIF data');
          end;
          OutCode^[OutCount] := Suffix^[CurCode];
          Inc(OutCount);
          CurCode := Prefix^[CurCode];
        end;

        FinalChar := CurCode and BitMask;
        OutCode^[OutCount] := FinalChar;
        Inc(OutCount);
        for I := OutCount - 1 downto 0 do
        begin
          Output(Byte(OutCode^[I]), OutCtxt);
        end;
        OutCount := 0;
        // Update dictionary
        if not TableFull then
        begin
          Prefix^[FreeCode] := OldCode;
          Suffix^[FreeCode] := FinalChar;
          // Advance to next free slot
          Inc(FreeCode);
          if FreeCode >= MaxCode then
          begin
            if ReadCtxt.CodeSize < 12 then
            begin
              Inc(ReadCtxt.CodeSize);
              MaxCode := MaxCode shl 1;
              ReadCtxt.ReadMask := (1 shl ReadCtxt.CodeSize) - 1;
            end
            else
              TableFull := True;
          end;
        end;
        OldCode := InCode;
      end;
      Code := ReadCode(ReadCtxt);
    end;
    if Code = $FFFF then
    begin
      raise EGIFImageError.Create('Error when decoding GIF data');
    end;
  finally
    FreeMem(Prefix);
    FreeMem(OutCode);
    FreeMem(Suffix);
  end;
end;

procedure FreeMemNil(var P);
begin
  FreeMem(Pointer(P));
  Pointer(P) := nil;
end;

function TestImage(const Image: TGIFPaletteImage): Boolean;
begin
  try
    Result := Image.Width * Image.Height = Image.Size;
  except
    Result := False;
  end;
end;

procedure InitImage(var Image: TGIFPaletteImage);
begin
  FillChar(Image, SizeOf(Image), 0);
end;

procedure FreeImage(var Image: TGIFPaletteImage);
begin
  try
    if TestImage(Image) then
    begin
      FreeMemNil(Image.Bits);
      FreeMemNil(Image.Palette);
    end;
    InitImage(Image);
  except
    raise EGIFImageError.Create('Error while freeing GIF image.');
  end;
end;

function NewImage(Width, Height: Integer; var Image: TGIFPaletteImage): Boolean;
begin
  Assert((Width > 0) and (Height >0));
  Result := False;
  FreeImage(Image);
  try
    Image.Width := Width;
    Image.Height := Height;
    Image.Size := Image.Width * Image.Height;
    if Image.Size = 0 then
    begin
      InitImage(Image);
      Exit;
    end;
    GetMem(Image.Bits, Image.Size);
    FillChar(Image.Bits^, Image.Size, 0);
    GetMem(Image.Palette, 256 * SizeOf(TAlphaColorRec));
    FillChar(Image.Palette^, 256 * SizeOf(TAlphaColorRec), 0);
    Result := TestImage(Image);
  except
    on E: Exception do
    begin
      FreeMem(Image.Bits);
      FreeMem(Image.Palette);
      InitImage(Image);
      raise EGIFImageError.Create(E.Message);
    end;
  end;
end;

function CloneImage(const Image: TGIFPaletteImage; var Clone: TGIFPaletteImage): Boolean;
begin
  Result := False;
  if TestImage(Image) then
  try
    if TestImage(Clone) and (Image.Bits <> Clone.Bits) then
    begin
      FreeImage(Clone);
    end
    else begin
      InitImage(Clone);
    end;
    Clone.Width := Image.Width;
    Clone.Height := Image.Height;
    Clone.Size := Image.Size;
    GetMem(Clone.Palette, 256 * SizeOf(TAlphaColorRec));
    Move(Image.Palette^, Clone.Palette^, 256 * SizeOf(TAlphaColorRec));
    GetMem(Clone.Bits, Clone.Size);
    Move(Image.Bits^, Clone.Bits^, Clone.Size);
    Result := True;
  except
    raise EGIFImageError.Create('Error while cloning GIF image.');
  end;
end;

function Pow2Int(Exponent: Integer): Integer;
begin
  Result := 1 shl Exponent;
end;

procedure FillCustomPalette(APal: PPalette32; AEntries: Integer; ARBits, AGBits, ABBits: Byte; AAlpha: Byte = $FF);
var
  I, TotalBits, MaxEntries: Integer;
begin
  Assert(APal <> nil);
  TotalBits := ARBits + AGBits + ABBits;
  MaxEntries := Min(Pow2Int(TotalBits), AEntries);
  FillChar(APal^, AEntries * SizeOf(TAlphaColorRec), 0);
  try
    for I := 0 to MaxEntries - 1 do
    with APal[I] do
    begin
      A := AAlpha;
      if ARBits > 0 then
        R := ((I shr Max(0, AGBits + ABBits - 1)) and (1 shl ARBits - 1)) * 255 div (1 shl ARBits - 1);
      if AGBits > 0 then
        G := ((I shr Max(0, ABBits - 1)) and (1 shl AGBits - 1)) * 255 div (1 shl AGBits - 1);
      if ABBits > 0 then
        B := ((I shr 0) and (1 shl ABBits - 1)) * 255 div (1 shl ABBits - 1);
    end;
  except
    raise EGIFImageError.Create('Error while filling custom palette entries.');
  end;
end;

procedure ClipCopyBounds(var SrcX, SrcY, Width, Height, DstX, DstY: Integer; SrcImageWidth, SrcImageHeight: Integer; const DstClip: TRect);

  procedure ClipDim(var SrcPos, DstPos, Size: Integer; SrcClipMax, DstClipMin, DstClipMax: Integer);
  var
    OldDstPos: Integer;
    Diff: Integer;
  begin
    if DstPos < 0 then OldDstPos := DstPos else OldDstPos := 0;
    if DstPos < DstClipMin then
    begin
      Diff := DstClipMin - DstPos;
      Size := Size - Diff;
      SrcPos := SrcPos + Diff;
      DstPos := DstClipMin;
    end;
    if SrcPos < 0 then
    begin
      Size := Size + SrcPos - OldDstPos;
      DstPos := DstPos - SrcPos + OldDstPos;
      SrcPos := 0;
    end;
    if SrcPos + Size > SrcClipMax then Size := SrcClipMax - SrcPos;
    if DstPos + Size > DstClipMax then Size := DstClipMax - DstPos;
  end;

begin
  ClipDim(SrcX, DstX, Width, SrcImageWidth, DstClip.Left, DstClip.Right);
  ClipDim(SrcY, DstY, Height, SrcImageHeight, DstClip.Top, DstClip.Bottom);
end;

procedure CopyImageToBitmap(const Image: TGIFPaletteImage; Bitmap: TBitmap; DstX, DstY: Integer);
var
  SrcX, SrcY, Width, Height : Integer;
  X, Y, SrcWidthBytes : Integer;
  DstPtr: PAlphaColor;
  SrcPtr: PByte;
  Data : TBitmapData;
begin
  Assert(TestImage(Image) and not Bitmap.IsEmpty);
  SrcX := 0;
  SrcY := 0;
  Width := Image.Width;
  Height := Image.Height;
  ClipCopyBounds(SrcX, SrcY, Width, Height, DstX, DstY, Image.Width, Image.Height, Rect(0, 0, Bitmap.Width, Bitmap.Height));
  SrcWidthBytes := Image.Width;
  SrcPtr := @PByteArray(Image.Bits)[SrcY * SrcWidthBytes + SrcX];
  Bitmap.Map(TMapAccess.ReadWrite, Data);
  try
    for Y := DstY to DstY + Height - 1 do
    begin
      DstPtr := PAlphaColor(Data.GetScanLine(Y));
      Inc(DstPtr, DstX);
      for X := 0 to Width - 1 do
      begin
        if Image.Palette[SrcPtr^].A <> 0 then
        begin
          AlphaColorToPixel(Image.Palette[SrcPtr^].Color, DstPtr, Bitmap.PixelFormat);
        end;
        Inc(DstPtr);
        Inc(SrcPtr);
      end;
    end;
  finally
    Bitmap.Unmap(Data);
  end;
end;

function CompareBytes(ABytes : array of Byte; AValue : String) : Boolean;
var
  Buffer : TBytes;
begin
  SetLength(Buffer, Length(ABytes));
  Move(ABytes[0], Buffer[0], Length(ABytes));
  Result := TEncoding.Ascii.GetString(Buffer, 0, Length(Buffer)) = AValue;
end;

{ **************************************************************************** }
{ TBufferedStream }
{ **************************************************************************** }
{ Based on TaaBufferedStream Copyright (c) Julian M Bucknall 1997, 1999 }
{ **************************************************************************** }

type
  TBufferedStream = class
  private const
    DefaultBufferSize = 16 * 1024;
  private
    FBuffer: PByteArray;
    FBufSize: Integer;
    FBufStart: Integer;
    FBufPos: Integer;
    FBytesInBuf: Integer;
    FSize: Integer;
    FDirty: Boolean;
    FStream: TStream;
    function GetPosition: Integer;
    function GetSize: Integer;
    procedure ReadBuffer;
    procedure WriteBuffer;
    procedure SetPosition(const Value: Integer);
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Integer): Integer;
    function Write(const Buffer; Count: Integer): Integer;
    function Seek(Offset: Integer; Origin: Word): Integer;
    procedure Commit;
    property Stream: TStream read FStream;
    property Position: Integer read GetPosition write SetPosition;
    property Size: Integer read GetSize;
  end;

constructor TBufferedStream.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
  FBufSize := DefaultBufferSize;
  GetMem(FBuffer, FBufSize);
  FBufPos := 0;
  FBytesInBuf := 0;
  FBufStart := 0;
  FDirty := False;
  FSize := AStream.Size;
end;

destructor TBufferedStream.Destroy;
begin
  if FBuffer <> nil then
  begin
    Commit;
    FreeMem(FBuffer);
  end;
  FStream.Position := Position; // Make sure source stream has right position
  inherited Destroy;
end;

function TBufferedStream.GetPosition: Integer;
begin
  Result := FBufStart + FBufPos;
end;

procedure TBufferedStream.SetPosition(const Value: Integer);
begin
  Seek(Value, soFromCurrent);
end;

function TBufferedStream.GetSize: Integer;
begin
  Result := FSize;
end;

procedure TBufferedStream.ReadBuffer;
var
  SeekResult: Integer;
begin
  SeekResult := FStream.Seek(FBufStart, 0);
  if SeekResult = -1 then
    raise Exception.Create('TBufferedStream.ReadBuffer: seek failed');
  FBytesInBuf := FStream.Read(FBuffer^, FBufSize);
  if FBytesInBuf <= 0 then
    raise Exception.Create('TBufferedStream.ReadBuffer: read failed');
end;

procedure TBufferedStream.WriteBuffer;
var
  SeekResult: Integer;
  BytesWritten: Integer;
begin
  SeekResult := FStream.Seek(FBufStart, 0);
  if SeekResult = -1 then
    raise Exception.Create('TBufferedStream.WriteBuffer: seek failed');
  BytesWritten := FStream.Write(FBuffer^, FBytesInBuf);
  if BytesWritten <> FBytesInBuf then
    raise Exception.Create('TBufferedStream.WriteBuffer: write failed');
end;

procedure TBufferedStream.Commit;
begin
  if FDirty then
  begin
    WriteBuffer;
    FDirty := False;
  end;
end;

function TBufferedStream.Read(var Buffer; Count: Integer): Integer;
var
  BufAsBytes  : TByteArray absolute Buffer;
  BufIdx, BytesToGo, BytesToRead: Integer;
begin
  // Calculate the actual number of bytes we can read - this depends on
  // the current position and size of the stream as well as the number
  // of bytes requested.
  BytesToGo := Count;
  if FSize < (FBufStart + FBufPos + Count) then
    BytesToGo := FSize - (FBufStart + FBufPos);

  if BytesToGo <= 0 then
  begin
    Result := 0;
    Exit;
  end;
  // Remember to return the result of our calculation
  Result := BytesToGo;

  BufIdx := 0;
  if FBytesInBuf = 0 then
    ReadBuffer;
  // Calculate the number of bytes we can read prior to the loop
  BytesToRead := FBytesInBuf - FBufPos;
  if BytesToRead > BytesToGo then
    BytesToRead := BytesToGo;
  // Copy from the stream buffer to the caller's buffer
  Move(FBuffer^[FBufPos], BufAsBytes[BufIdx], BytesToRead);
  // Calculate the number of bytes still to read}
  Dec(BytesToGo, BytesToRead);

  // while we have bytes to read, read them
  while BytesToGo > 0 do
  begin
    Inc(BufIdx, BytesToRead);
    // As we've exhausted this buffer-full, advance to the next, check
    //  to see whether we need to write the buffer out first
    if FDirty then
    begin
      WriteBuffer;
      FDirty := false;
    end;
    Inc(FBufStart, FBufSize);
    FBufPos := 0;
    ReadBuffer;
    // Calculate the number of bytes we can read in this cycle
    BytesToRead := FBytesInBuf;
    if BytesToRead > BytesToGo then
      BytesToRead := BytesToGo;
    // Ccopy from the stream buffer to the caller's buffer
    Move(FBuffer^, BufAsBytes[BufIdx], BytesToRead);
    // Calculate the number of bytes still to read
    Dec(BytesToGo, BytesToRead);
  end;
  // Remember our new position
  Inc(FBufPos, BytesToRead);
  if FBufPos = FBufSize then
  begin
    Inc(FBufStart, FBufSize);
    FBufPos := 0;
    FBytesInBuf := 0;
  end;
end;

function TBufferedStream.Seek(Offset: Integer; Origin: Word): Integer;
var
  NewBufStart, NewPos: Integer;
begin
  // Calculate the new position
  case Origin of
    soFromBeginning : NewPos := Offset;
    soFromCurrent   : NewPos := FBufStart + FBufPos + Offset;
    soFromEnd       : NewPos := FSize + Offset;
  else
    raise Exception.Create('TBufferedStream.Seek: invalid origin');
  end;

  if (NewPos < 0) or (NewPos > FSize) then
  begin
    //NewPos := ClampInt(NewPos, 0, FSize); don't do this - for writing
  end;
  // Calculate which page of the file we need to be at
  NewBufStart := NewPos and not Pred(FBufSize);
  // If the new page is different than the old, mark the buffer as being
  // ready to be replenished, and if need be write out any dirty data
  if NewBufStart <> FBufStart then
  begin
    if FDirty then
    begin
      WriteBuffer;
      FDirty := False;
    end;
    FBufStart := NewBufStart;
    FBytesInBuf := 0;
  end;
  // Save the new position
  FBufPos := NewPos - NewBufStart;
  Result := NewPos;
end;

function TBufferedStream.Write(const Buffer; Count: Integer): Integer;
var
  BufAsBytes: TByteArray absolute Buffer;
  BufIdx, BytesToGo, BytesToWrite: Integer;
begin
  // When we write to this stream we always assume that we can write the
  // requested number of bytes: if we can't (eg, the disk is full) we'll
  // get an exception somewhere eventually.
  BytesToGo := Count;
  // Remember to return the result of our calculation
  Result := BytesToGo;

  BufIdx := 0;
  if (FBytesInBuf = 0) and (FSize > FBufStart) then
    ReadBuffer;
  // Calculate the number of bytes we can write prior to the loop
  BytesToWrite := FBufSize - FBufPos;
  if BytesToWrite > BytesToGo then
    BytesToWrite := BytesToGo;
  // Copy from the caller's buffer to the stream buffer
  Move(BufAsBytes[BufIdx], FBuffer^[FBufPos], BytesToWrite);
  // Mark our stream buffer as requiring a save to the actual stream,
  // note that this will suffice for the rest of the routine as well: no
  // inner routine will turn off the dirty flag.
  FDirty := True;
  // Calculate the number of bytes still to write
  Dec(BytesToGo, BytesToWrite);

  // While we have bytes to write, write them
  while BytesToGo > 0 do
  begin
    Inc(BufIdx, BytesToWrite);
    // As we've filled this buffer, write it out to the actual stream
    // and advance to the next buffer, reading it if required
    FBytesInBuf := FBufSize;
    WriteBuffer;
    Inc(FBufStart, FBufSize);
    FBufPos := 0;
    FBytesInBuf := 0;
    if FSize > FBufStart then
      ReadBuffer;
    // Calculate the number of bytes we can write in this cycle
    BytesToWrite := FBufSize;
    if BytesToWrite > BytesToGo then
      BytesToWrite := BytesToGo;
    // Copy from the caller's buffer to our buffer
    Move(BufAsBytes[BufIdx], FBuffer^, BytesToWrite);
    // Calculate the number of bytes still to write
    Dec(BytesToGo, BytesToWrite);
  end;
  // Remember our new position
  Inc(FBufPos, BytesToWrite);
  // Make sure the count of valid bytes is correct
  if FBytesInBuf < FBufPos then
    FBytesInBuf := FBufPos;
  // Make sure the stream size is correct
  if FSize < (FBufStart + FBytesInBuf) then
    FSize := FBufStart + FBytesInBuf;
  // If we're at the end of the buffer, write it out and advance to the
  // start of the next page
  if FBufPos = FBufSize then
  begin
    WriteBuffer;
    FDirty := False;
    Inc(FBufStart, FBufSize);
    FBufPos := 0;
    FBytesInBuf := 0;
  end;
end;

{ **************************************************************************** }
{ TGIFFrame }
{ **************************************************************************** }

constructor TGIFData.TGIFFrame.Create;
begin
  inherited Create;
end;

destructor TGIFData.TGIFFrame.Destroy;
begin
  FreeImage(FData);
  inherited Destroy;
end;

{ **************************************************************************** }
{ TGIFData }
{ **************************************************************************** }

constructor TGIFData.Create;
begin
  inherited Create;
  FStream := TMemoryStream.Create;
  FFrames := TGIFFrames.Create;
  FCachedFrame := TBitmap.Create;
  FCachedIndex := -1;
end;

destructor TGIFData.Destroy;
begin
  InternalClear;
  FreeAndNil(FStream);
  FreeAndNil(FFrames);
  FreeAndNil(FCachedFrame);
  inherited Destroy;
end;

function TGIFData.GetFrame(const AIndex : Integer) : TGIFFrame;
begin
  Result := FFrames[AIndex];
end;

function TGIFData.GetFrameCount : Integer;
begin
  Result := FFrames.Count;
end;

procedure TGIFData.Assign(Source : TPersistent);
begin
  if Source is TGIFData then
  begin
    if not TGIFData(Source).EqualsTo(Self) then
    begin
      LoadFromStream(TGIFData(Source).Stream);
    end;
  end
  else begin
    inherited;
  end;
end;

procedure TGIFData.LoadFromStream(AStream : TStream);
var
  Buffer : TBufferedStream;
  ScreenWidth, ScreenHeight, I : Integer;
  BlockID: Byte;
  HasGraphicExt: Boolean;
  GraphicExt: TGIFGraphicControlExtension;
  AppRead: Boolean;

  function ReadBlockID: Byte;
  begin
    Result := GIF_Trailer;
    if Buffer.Read(Result, SizeOf(Result)) < SizeOf(Result) then
    begin
      Result := GIF_Trailer;
    end;
  end;

  procedure ReadExtensions;
  var
    BlockSize, BlockType, ExtType: Byte;
    AppRec: TGIFApplicationRec;
    LoopCount: SmallInt;

    procedure SkipBytes;
    begin
      repeat
        Buffer.Read(BlockSize, SizeOf(BlockSize));
        Buffer.Seek(BlockSize, soFromCurrent);
      until BlockSize = 0;
    end;

  begin
    HasGraphicExt := False;
    AppRead := False;

    while BlockID = GIF_ExtensionIntroducer do
    begin
      Buffer.Read(ExtType, SizeOf(ExtType));

      while ExtType in [GIF_GraphicExtension, GIF_CommentExtension, GIF_ApplicationExtension, GIF_PlainText] do
      begin
        if ExtType = GIF_GraphicExtension then
        begin
          HasGraphicExt := True;
          Buffer.Read(GraphicExt, SizeOf(GraphicExt));
        end
        else if (ExtType = GIF_ApplicationExtension) and not AppRead then
        begin
          Buffer.Read(BlockSize, SizeOf(BlockSize));
          if BlockSize >= SizeOf(AppRec) then
          begin
            Buffer.Read(AppRec, SizeOf(AppRec));
            if (CompareBytes(AppRec.Identifier, 'NETSCAPE') and CompareBytes(AppRec.Authentication, '2.0')) or
               (CompareBytes(AppRec.Identifier, 'ANIMEXTS') and CompareBytes(AppRec.Authentication, '1.0')) then
            begin
              Buffer.Read(BlockSize, SizeOf(BlockSize));
              while BlockSize <> 0 do
              begin
                BlockType := ReadBlockID;
                Dec(BlockSize);

                case BlockType of
                  GIF_AppLoopExtension:
                    if (BlockSize >= SizeOf(LoopCount)) then
                    begin
                      Buffer.Read(LoopCount, SizeOf(LoopCount));
                      Dec(BlockSize, SizeOf(LoopCount));
                      if LoopCount > 0 then Inc(LoopCount);
                    end;
                  GIF_AppBufferExtension:
                    begin
                      Dec(BlockSize, SizeOf(Word));
                      Buffer.Seek(SizeOf(Word), soFromCurrent);
                    end;
                end;
              end;
              SkipBytes;
              AppRead := True;
            end
            else begin
              Buffer.Seek(-SizeOf(AppRec) - SizeOf(BlockSize), soFromCurrent);
              SkipBytes;
            end;
          end
          else begin
            Buffer.Seek(- BlockSize - SizeOf(BlockSize), soFromCurrent);
            SkipBytes;
          end;
        end
        else if ExtType in [GIF_CommentExtension, GIF_ApplicationExtension, GIF_PlainText] then
        repeat
          Buffer.Read(BlockSize, SizeOf(BlockSize));
          Buffer.Seek(BlockSize, soFromCurrent);
        until BlockSize = 0;

        BlockID := ReadBlockID;
        ExtType := BlockID;
      end
    end;
  end;

  procedure CopyLZWData(Dest: TStream);
  var
    CodeSize, BlockSize: Byte;
    InputSize: Integer;
    Buff: array[Byte] of Byte;
  begin
    InputSize := Buffer.Size;
    Buffer.Read(CodeSize, 1);
    Dest.Write(CodeSize, 1);
    repeat
      Buffer.Read(BlockSize, 1);
      Dest.Write(BlockSize, 1);
      if BlockSize > 0 then
      begin
        Buffer.Read(Buff[0], BlockSize);
        Dest.Write(Buff[0], BlockSize);
      end;
    until (BlockSize = 0) or (Buffer.Position >= InputSize);
  end;

  procedure ReadFrame;
  var
    ImageDesc: TGIFImageDescriptor;
    NewFrame : TGIFFrame;
    Interlaced: Boolean;
    I, LocalPalLength: Integer;
    LocalPal: TPalette32Size256;
    LZWStream: TMemoryStream;
  begin

    NewFrame := TGIFFrame.Create;
    FFrames.Add(NewFrame);
    FillChar(LocalPal, SizeOf(LocalPal), 0);
    Buffer.Read(ImageDesc, SizeOf(ImageDesc));
    NewFrame.FHasLocalPal := (ImageDesc.PackedFields and GIF_LocalColorTable) = GIF_LocalColorTable;
    Interlaced := (ImageDesc.PackedFields and GIF_Interlaced) = GIF_Interlaced;
    LocalPalLength := ImageDesc.PackedFields and GIF_ColorTableSize;
    LocalPalLength := 1 shl (LocalPalLength + 1);
    if (ImageDesc.Width = 0) or (ImageDesc.Width > FHeader.ScreenWidth) then
    begin
      ImageDesc.Width := FHeader.ScreenWidth;
    end;
    if (ImageDesc.Height = 0) or (ImageDesc.Height > FHeader.ScreenHeight)  then
    begin
      ImageDesc.Height := FHeader.ScreenHeight;
    end;
    NewFrame.FLeft := ImageDesc.Left;
    NewFrame.FTop := ImageDesc.Top;
    NewFrame.FWidth := ImageDesc.Width;
    NewFrame.FHeight := ImageDesc.Height;
    NewFrame.FBackIndex := FHeader.BackColorIndex;
    NewImage(ImageDesc.Width, ImageDesc.Height, NewFrame.FData);
    if NewFrame.FHasLocalPal then
    begin
      for I := 0 to LocalPalLength - 1 do
      begin
        LocalPal[I].A := 255;
        Buffer.Read(LocalPal[I].R, SizeOf(LocalPal[I].R));
        Buffer.Read(LocalPal[I].G, SizeOf(LocalPal[I].G));
        Buffer.Read(LocalPal[I].B, SizeOf(LocalPal[I].B));
      end;
    end;

    if NewFrame.FHasLocalPal then
    begin
      Move(LocalPal, NewFrame.FData.Palette^, SizeOf(LocalPal))
    end
    else if FHasPalette then
    begin
      Move(FPalette, NewFrame.FData.Palette^, SizeOf(FPalette))
    end
    else begin
      FillCustomPalette(NewFrame.FData.Palette, FPaletteLength, 3, 3, 2);
    end;

    if (ImageDesc.Left <= FHeader.ScreenWidth + 1) and (ImageDesc.Top <= FHeader.ScreenHeight + 1) then
    begin
      ScreenWidth := Max(ScreenWidth, ImageDesc.Width + ImageDesc.Left);
      ScreenHeight := Max(ScreenHeight, ImageDesc.Height + ImageDesc.Top);
    end
    else begin
      FreeAndNil(NewFrame);
      Exit;
    end;

    if HasGraphicExt then
    begin
      NewFrame.FHasTransparency := (GraphicExt.PackedFields and GIF_Transparent) = GIF_Transparent;
      NewFrame.FDisposal := TGIFDisposalMethod((GraphicExt.PackedFields and GIF_DisposalMethod) shr 2);
      if NewFrame.FHasTransparency then
      begin
        NewFrame.FTransIndex := GraphicExt.TransparentColorIndex;
        NewFrame.FData.Palette[NewFrame.FTransIndex].A := 0;
      end;
      if GraphicExt.DelayTime > 0 then
      begin
        NewFrame.FDelay := Integer(GraphicExt.DelayTime * 10);
      end
      else begin
        NewFrame.FDelay := GIF_DefaultDelay;
      end;
    end
    else begin
      NewFrame.FHasTransparency := False;
    end;
    LZWStream := TMemoryStream.Create;
    try
      try
        CopyLZWData(LZWStream);
        LZWStream.Position := 0;
        LZWDecompress(LZWStream, ImageDesc.Width, ImageDesc.Height, Interlaced, NewFrame.FData.Bits);
      except
        FreeAndNil(NewFrame);
        Exit;
      end;
    finally
      LZWStream.Free;
    end;
  end;

begin
  Buffer := TBufferedStream.Create(AStream);
  try
    Clear;
    FillChar(FPalette, SizeOf(FPalette), 0);

    with Buffer do
    begin
      Read(FHeader, SizeOf(FHeader));
      ScreenWidth := FHeader.ScreenWidth;
      ScreenHeight := FHeader.ScreenHeight;
      FHasPalette := FHeader.PackedFields and GIF_GlobalColorTable = GIF_GlobalColorTable;
      FPaletteLength := FHeader.PackedFields and GIF_ColorTableSize;
      FPaletteLength := 1 shl (FPaletteLength + 1);
      if FHasPalette then
      begin
        for I := 0 to FPaletteLength - 1 do
        begin
          FPalette[I].A := 255;
          Read(FPalette[I].R, SizeOf(FPalette[I].R));
          Read(FPalette[I].G, SizeOf(FPalette[I].G));
          Read(FPalette[I].B, SizeOf(FPalette[I].B));
        end;
      end;
      BlockID := ReadBlockID;
      while BlockID <> GIF_Trailer do
      begin
        while not (BlockID in [GIF_Trailer, GIF_ExtensionIntroducer, GIF_ImageDescriptor]) do
        begin
          BlockID := ReadBlockID;
        end;
        ReadExtensions;
        if BlockID = GIF_ImageDescriptor then
        begin
          ReadFrame;
        end;
        BlockID := ReadBlockID;
        if not (BlockID in [GIF_ExtensionIntroducer, GIF_Trailer, GIF_ImageDescriptor]) then
        begin
          BlockID := GIF_Trailer;
        end;
      end;
    end;
  finally
    FreeAndNil(Buffer);
    AStream.Seek(0, soFromBeginning);
    FStream.CopyFrom(AStream, AStream.Size);
    DoChange;
  end;
end;

procedure TGIFData.InternalClear;
begin
  FStream.Clear;
  FFrames.Clear;
  FCachedIndex := -1;
  FCachedFrame.SetSize(0, 0);
end;

procedure TGIFData.Clear;
begin
  InternalClear;
  DoChange;
end;

procedure TGIFData.LoadFromFile(const AFilename : String);
var
  Stream : TFileStream;
begin
  Stream := TFileStream.Create(AFilename, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TGIFData.InternalRender(AIndex: Integer; const ABitmap: TBitmap; const ACached : TBitmap; var ACachedIndex : Integer);
var
  I, First, Last: Integer;
  UseCache: Boolean;
  BGColor: TAlphaColor;
begin
  Last := AIndex;
  First := Max(0, Last);
  UseCache := Assigned(ACached) and (not ACached.IsEmpty) and (ACachedIndex = AIndex - 1) and (ACachedIndex >= 0) and (FFrames[ACachedIndex].Disposal <> dmRestorePrevious);
  if UseCache then
  begin
    ABitmap.Assign(ACached);
  end
  else if Assigned(ACached) then
  begin
    ACached.SetSize(0, 0);
  end;
  BGColor := FFrames[AIndex].Data.Palette[FFrames[AIndex].BackIndex].Color;
  if not UseCache then
  begin
    if FFrames[AIndex].HasTransparency then
    begin
      BGColor := $00000000;
    end;
    ABitmap.Clear(BGColor);
    while First > 0 do
    begin
      if (Header.ScreenWidth = FFrames[First].Width) and (Header.ScreenHeight = FFrames[First].Height) then
      begin
        if (FFrames[First].Disposal = dmRestoreBackground) and (First < Last) then
        begin
          Break;
        end;
      end;
      Dec(First);
    end;
    for I := First to Last - 1 do
    begin
      case FFrames[I].Disposal of
        dmNoRemoval, dmLeave:
        begin
          CopyImageToBitmap(FFrames[I].Data, ABitmap, FFrames[I].Left, FFrames[I].Top);
        end;
        dmRestoreBackground:
        begin
          if (I > First) then
          begin
            ABitmap.ClearRect(RectF(FFrames[I].Left, FFrames[I].Top, FFrames[I].Left + FFrames[I].Width, FFrames[I].Top + FFrames[I].Height), BGColor);
          end;
        end;
        dmRestorePrevious:;
      end;
    end;
  end
  else if FFrames[ACachedIndex].Disposal = dmRestoreBackground then
  begin
    if FFrames[ACachedIndex].HasTransparency then
    begin
      BGColor := $00000000;
    end;
    ABitmap.ClearRect(RectF(FFrames[ACachedIndex].Left, FFrames[ACachedIndex].Top, FFrames[ACachedIndex].Left + FFrames[ACachedIndex].Width, FFrames[ACachedIndex].Top + FFrames[ACachedIndex].Height), BGColor);
  end;
  CopyImageToBitmap(FFrames[AIndex].Data, ABitmap, FFrames[AIndex].Left, FFrames[AIndex].Top);
  if Assigned(ACached) then
  begin
    ACached.Assign(ABitmap);
    ACachedIndex := AIndex;
  end;
end;

procedure TGIFData.Render(AFrameIndex : Integer; ABitmap : TBitmap);
begin
  if (AFrameIndex >= 0) and (AFrameIndex <= FrameCount - 1) then
  begin
    if AFrameIndex = FCachedIndex then
    begin
      ABitmap.Assign(FCachedFrame);
    end
    else begin
      ABitmap.SetSize(FHeader.ScreenWidth, FHeader.ScreenHeight);
      ABitmap.Clear(0);
      InternalRender(AFrameIndex, ABitmap, FCachedFrame, FCachedIndex);
    end;
  end
  else begin
    ABitmap.SetSize(0, 0);
  end;
end;

function TGIFData.EqualsTo(AData : TGIFData) : Boolean;
begin
  Result := AData.Stream.Size = FStream.Size;
  if Result then
  begin
    Result := CompareMem(AData.Stream.Memory, FStream.Memory, FStream.Size);
  end;
end;

function TGIFData.IsEmpty : Boolean;
begin
  Result := FStream.Size = 0;
end;

procedure TGIFData.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not (Filer.Ancestor is TGIFData) or not EqualsTo(TGIFData(Filer.Ancestor))
    else
      Result := not IsEmpty;
  end;

begin
  inherited;
  Filer.DefineBinaryProperty('GIF', LoadFromStream, SaveToStream, DoWrite);
end;

procedure TGIFData.DoChange;
begin
end;

procedure TGIFData.SaveToStream(AStream : TStream);
begin
  FStream.SaveToStream(AStream);
end;

procedure TGIFData.SaveToFile(const AFilename : String);
begin
  FStream.SaveToFile(AFilename);
end;

{ **************************************************************************** }
{ TGIFImageData }
{ **************************************************************************** }

type
  TGIFImageData = class(TGIFData)
  private
    FImage : TGIFImage;
  protected
    procedure DoChange; override;
  public
    constructor Create(AImage : TGIFImage); reintroduce;
  end;

constructor TGIFImageData.Create(AImage : TGIFImage);
begin
  inherited Create;
  FImage := AImage;
end;

procedure TGIFImageData.DoChange;
begin
  FImage.DoImageChanged;
end;

{ **************************************************************************** }
{ TGIFImage }
{ **************************************************************************** }

constructor TGIFImage.Create(AOwner: TComponent);
begin
  inherited;
  FGIFData        := TGIFImageData.Create(Self);
  FBitmap         := TBitmap.Create;
  FFill           := TBrush.Create(TBrushKind.None, TAlphaColors.White);
  FFill.OnChanged := DoFillChanged;
  FWrapMode       := TImageWrapMode.Fit;
  FPlayer := TTimer.Create(nil);
  FPlayer.Enabled := False;
  FPlayer.OnTimer := DoRenderNextFrame;
  FActiveFrame    := 0;
  FSpeed          := 1;
  FLoop           := True;
  FAutoPlay       := True;
  SetAcceptsControls(False);
  if Scene <> nil then
  begin
    FScreenScale := Scene.GetSceneScale;
  end
  else begin
    FScreenScale := 1.0;
  end;
  FScaleChangedId := TMessageManager.DefaultManager.SubscribeToMessage(TScaleChangedMessage, ScaleChangedHandler);
end;

destructor TGIFImage.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TScaleChangedMessage, FScaleChangedId);
  FreeAndNil(FPlayer);
  FreeAndNil(FGIFData);
  FreeAndNil(FBitmap);
  FreeAndNil(FFill);
  inherited;
end;

procedure TGIFImage.DoImageChanged;
begin
  if not (csLoading in ComponentState) then
  begin
    Stop;
    ActiveFrame := 0;
  end;
  DoChange;
end;

procedure TGIFImage.SetFill(const AValue: TBrush);
begin
  FFill.Assign(AValue);
end;

procedure TGIFImage.DoFillChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TGIFImage.DoPlay;
begin
  if Assigned(FOnPlay) then FOnPlay(Self);
end;

procedure TGIFImage.DoStop;
begin
  if Assigned(FOnStop) then FOnStop(Self);
end;

procedure TGIFImage.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TGIFImage.DrawBitmap(const Canvas: TCanvas; const ARect: TRectF; const ABitmap: TBitmap; const AOpacity: Single = 1.0);
var
  LR, R, IntersectionRect: TRectF;
  I, J: Integer;
begin
  LR := TRectF.Create(ARect.Left * FScreenScale, ARect.Top * FScreenScale, ARect.Right * FScreenScale, ARect.Bottom * FScreenScale);

  case FWrapMode of
    TImageWrapMode.Original:
    begin
      R := TRectF.Create(ARect.Left, ARect.Top, ARect.Left + ABitmap.Width, ARect.Top + ABitmap.Height);
      IntersectRect(IntersectionRect, LR, R);
      Canvas.DrawBitmap(ABitmap, TRectF.Create(0, 0, IntersectionRect.Width, IntersectionRect.Height), TRectF.Create(R.Left, R.Top, R.Left + IntersectionRect.Width / FScreenScale, R.Top + IntersectionRect.Height / FScreenScale), AOpacity, DisableInterpolation)
    end;
    TImageWrapMode.Fit:
    begin
      R := TRectF.Create(0, 0, ABitmap.Width / ABitmap.BitmapScale, ABitmap.Height / ABitmap.BitmapScale);
      R := R.FitInto(ARect).SnapToPixel(Canvas.Scale, False);
      Canvas.DrawBitmap(ABitmap, TRectF.Create(0, 0, ABitmap.Width, ABitmap.Height), R, AOpacity, DisableInterpolation);
    end;
    TImageWrapMode.Stretch:
    begin
      Canvas.DrawBitmap(ABitmap, TRectF.Create(0, 0, ABitmap.Width, ABitmap.Height), ARect, AOpacity, DisableInterpolation)
    end;
    TImageWrapMode.Tile:
    begin
      for I := 0 to Trunc(LR.Width / ABitmap.Width) + 1 do
        for J := 0 to Trunc(LR.Height / ABitmap.Height) + 1 do
        begin
          R := TRectF.Create(0, 0, ABitmap.Width, ABitmap.Height);
          OffsetRect(R, I * ABitmap.Width, J * ABitmap.Height);
          IntersectRect(IntersectionRect, LR, R);
          Canvas.DrawBitmap(ABitmap, TRectF.Create(0, 0, IntersectionRect.Width, IntersectionRect.Height),
            TRectF.Create(R.Left / FScreenScale, R.Top / FScreenScale, (R.Left + IntersectionRect.Width) / FScreenScale,
            (R.Top + IntersectionRect.Height) / FScreenScale), AOpacity, True)
        end;
    end;
    TImageWrapMode.Center:
    begin
      R := TRectF.Create(0, 0, ABitmap.Width / ABitmap.BitmapScale, ABitmap.Height / ABitmap.BitmapScale);
      R := R.CenterAt(ARect).SnapToPixel(Canvas.Scale, False);
      Canvas.DrawBitmap(ABitmap, TRectF.Create(0, 0, ABitmap.Width, ABitmap.Height), R, AOpacity, DisableInterpolation);
    end;
    TImageWrapMode.Place:
    begin
      R := TRectF.Create(0, 0, ABitmap.Width / ABitmap.BitmapScale, ABitmap.Height / ABitmap.BitmapScale);
      R := R.PlaceInto(ARect).SnapToPixel(Canvas.Scale, False);
      Canvas.DrawBitmap(ABitmap, TRectF.Create(0, 0, ABitmap.Width, ABitmap.Height), R, AOpacity, DisableInterpolation);
    end;
  end;
end;

procedure TGIFImage.Paint;
var
  R: TRectF;
begin
  R := LocalRect;
  if FFill.Kind <> TBrushKind.None then
  begin
    Canvas.FillRect(R, 0, 0, [], AbsoluteOpacity, FFill);
  end;
  DrawBitmap(Canvas, R, FBitmap, AbsoluteOpacity);
  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    InflateRect(R, -0.5, -0.5);
    Canvas.DrawDashRect(R, 0, 0, AllCorners, AbsoluteOpacity, $A0909090);
  end;
end;

procedure TGIFImage.ScaleChangedHandler(const Sender: TObject; const Msg: TMessage);
begin
  Repaint;
end;

procedure TGIFImage.SetBitmap(const AValue: TBitmap);
begin
  FBitmap.Assign(AValue);
end;

procedure TGIFImage.SetGIFData(const AValue: TGIFData);
begin
  FGIFData.Assign(AValue);
end;

procedure TGIFImage.SetWrapMode(const AValue: TImageWrapMode);
begin
  if FWrapMode <> AValue then
  begin
    FWrapMode := AValue;
    Repaint;
  end;
end;

function TGIFImage.GetIsEmpty : Boolean;
begin
  Result := FGIFData.IsEmpty;
end;

procedure TGIFImage.Loaded;
begin
  inherited;
  if not IsEmpty then
  begin
    FActiveFrame := Max(0, Min(FActiveFrame, FGIFData.FrameCount - 1));
    FGIFData.Render(FActiveFrame, FBitmap);
    if Visible and AutoPlay then
    begin
      Play;
    end;
  end;
end;

procedure TGIFImage.VisibleChanged;
begin
  inherited;
  if not Visible then
  begin
    if IsPlaying then
    begin
      Stop;
    end;
  end
  else if not IsEmpty then
  begin
    FActiveFrame := Max(0, Min(FActiveFrame, FGIFData.FrameCount - 1));
    FGIFData.Render(FActiveFrame, FBitmap);
    if AutoPlay then
    begin
      Play;
    end;
  end;
end;

procedure TGIFImage.SetIsPlaying(const AValue: Boolean);
begin
  if AValue <> GetIsPlaying then
  begin
    if AValue then
    begin
      Play;
    end
    else begin
      Stop;
    end;
  end;
end;

function TGIFImage.GetIsPlaying : Boolean;
begin
  Result := FPlayer.Enabled;
end;

procedure TGIFImage.Play;
begin
  if not (csDesigning in ComponentState) and not IsPlaying and (FGIFData.FrameCount > 1) and not FGIFData.IsEmpty then
  begin
    FPlayer.Interval := 1;
    FPlayer.Enabled := True;
    DoPlay;
  end;
end;

procedure TGIFImage.Stop;
begin
  FPlayer.Enabled := False;
  DoStop;
end;

function TGIFImage.IsSpeedStored : Boolean;
begin
  Result := FSpeed <> 1.0;
end;

procedure TGIFImage.SetSpeed(const AValue: Single);
begin
  FSpeed := Max(AValue, 0.1);
end;

procedure TGIFImage.SetActiveFrame(const AValue: Integer);
begin
  if csLoading in ComponentState then
  begin
    FActiveFrame := AValue;
  end
  else begin
    FActiveFrame := Max(0, Min(AValue, FGIFData.FrameCount - 1));
    FGIFData.Render(FActiveFrame, FBitmap);
    Repaint;
  end;
end;

procedure TGIFImage.DoRenderNextFrame(Sender: TObject);
var
  StopWatch : TStopWatch;
begin
  FActiveFrame := Max(0, Min(FActiveFrame, FGIFData.FrameCount - 1)) + 1;
  if FActiveFrame > FGIFData.FrameCount - 1 then
  begin
    FActiveFrame := 0;
    if not FLoop then
    begin
      FPlayer.Enabled := False;
      DoStop;
      Exit;
    end;
  end;
  StopWatch := TStopWatch.StartNew;
  FGIFData.Render(FActiveFrame, FBitmap);
  Repaint;
  FPlayer.Interval := Max(1, Round((FGIFData.Frames[FActiveFrame].Delay - StopWatch.ElapsedMilliseconds) / Speed));
end;

procedure TGIFImage.LoadFromStream(AStream : TStream);
begin
  FGIFData.LoadFromStream(AStream);
end;

procedure TGIFImage.SaveToStream(AStream : TStream);
begin
  FGIFData.SaveToStream(AStream);
end;

procedure TGIFImage.LoadFromFile(const AFilename : String);
begin
  FGIFData.LoadFromFile(AFilename);
end;

procedure TGIFImage.SaveToFile(const AFilename : String);
begin
  FGIFData.SaveToFile(AFilename);
end;

end.
