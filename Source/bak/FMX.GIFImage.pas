unit FMX.GIFImage;

interface

uses
  System.Types,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.Threading,
  System.UITypes,
  System.Messaging,
  FMX.Types,
  FMX.Controls,
  FMX.Objects,
  FMX.Graphics;

type
  TGIFDisposalMethod = (dmNoRemoval, dmLeave, dmRestoreBackground, dmRestorePrevious, dmReserved4, dmReserved5, dmReserved6, dmReserved7);

  TImageFormat =
  (
    ifUnknown  = 0,
    ifIndex8   = 1,
    ifA8R8G8B8 = 2
  );
  TImageFormats = set of TImageFormat;

  TColor32 = LongWord;
  PColor32 = ^TColor32;
  TColor24Rec = packed record
    case LongInt of
      0: (B, G, R: Byte);
      1: (Channels: array[0..2] of Byte);
  end;
  PColor24Rec = ^TColor24Rec;
  TColor24RecArray = array[0..MaxInt div SizeOf(TColor24Rec) - 1] of TColor24Rec;
  PColor24RecArray = ^TColor24RecArray;

  TColor32Rec = packed record
    case LongInt of
      0: (Color: TColor32);
      1: (B, G, R, A: Byte);
      2: (Channels: array[0..3] of Byte);
      3: (Color24Rec: TColor24Rec);
  end;
  PColor32Rec = ^TColor32Rec;
  TColor32RecArray = array[0..MaxInt div SizeOf(TColor32Rec) - 1] of TColor32Rec;
  PColor32RecArray = ^TColor32RecArray;

  { Palette for indexed mode images with 32 bit colors.}
  TPalette32 = TColor32RecArray;
  TPalette32Size256 = array[0..255] of TColor32Rec;
  PPalette32 = ^TPalette32;

  { Record that stores single image data and information describing it.}
  TImageData = packed record
    Width   : LongInt;       // Width of image in pixels
    Height  : LongInt;      // Height of image in pixels
    Format  : TImageFormat; // Data format of image
    Delay   : Integer;
    Size    : LongInt;        // Size of image bits in Bytes
    Bits    : Pointer;        // Pointer to memory containing image bits
    Palette : PPalette32;  // Image palette for indexed images
    Tag     : Pointer;         // User data
  end;
  PImageData = ^TImageData;

  TChar3 = array[0..2] of Byte;

  TGIFData = class(TPersistent)
  private type

    TGIFHeader = packed record
      Signature: TChar3;
      Version: TChar3;
      ScreenWidth: Word;
      ScreenHeight: Word;
      PackedFields: Byte;
      BackgroundColorIndex: Byte;
      AspectRatio : Byte;
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
      FData            : TImageData;
    protected
      property Data : TImageData read FData;
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
    FStream      : TMemoryStream;
    FHeader      : TGIFHeader;
    FFrames      : TGIFFrames;
    FCachedIndex : Integer;
    FCachedImage : TImageData;
    FPlayer      : ITask;
    FOnPlay      : TNotifyEvent;
    FOnStop      : TNotifyEvent;
    FOnChange    : TNotifyEvent;
    function GetFrame(const AIndex : Integer) : TGIFFrame;
    function GetFrameCount : Integer;
    procedure InternalClear;
    procedure InternalRender(AIndex: Integer; var AFrame: TImageData);
  protected
    procedure DoPlayStart; virtual;
    procedure DoPlayStop; virtual;
    procedure DoChange; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    property Stream : TMemoryStream read FStream;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
    function EqualsTo(AData : TGIFData) : Boolean;
    function IsEmpty : Boolean;
    procedure LoadFromStream(AStream : TStream);
    procedure SaveToStream(AStream : TStream);
    procedure LoadFromFile(const AFilename : String);
    procedure SavetoFile(const AFilename : String);
    procedure Render(ABitmap : TBitmap; AFrameIndex : Integer);
    procedure Play(ABitmap : TBitmap);
    procedure Stop;
    procedure Clear;
    property Header : TGIFHeader read FHeader;
    property Frames[const Index : Integer] : TGIFFrame read GetFrame;
    property FrameCount : Integer read GetFrameCount;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property OnPlay : TNotifyEvent read FOnPlay write FOnPlay;
    property OnStop : TNotifyEvent read FOnStop write FOnStop;
  end;

  TGIFImage = class(TControl)
  private
    FWrapMode: TImageWrapMode;
    FDisableInterpolation: Boolean;
    FScaleChangedId: Integer;
    FScreenScale: Single;
    FBitmap  : TBitmap;
    FGIFData : TGIFData;
    procedure SetBitmap(const AValue: TBitmap);
    procedure SetGIFData(const AValue : TGIFData);
    procedure SetWrapMode(const AValue: TImageWrapMode);
    procedure ScaleChangedHandler(const Sender: TObject; const Msg: TMessage);
    procedure DoBitmapChanged(Sender : TObject);
  protected
    procedure DoChanged; virtual;
    procedure Paint; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawBitmap(const Canvas: TCanvas; const ARect: TRectF; const ABitmap: TBitmap; const AOpacity: Single = 1.0);
    property Bitmap: TBitmap read FBitmap write SetBitmap;
  published
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DisableInterpolation: Boolean read FDisableInterpolation write FDisableInterpolation default False;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property GIFData : TGIFData read FGIFData write SetGIFData;
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
    property WrapMode: TImageWrapMode read FWrapMode write SetWrapMode default TImageWrapMode.Fit;
    property ParentShowHint;
    property ShowHint;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Mouse events}
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
    property OnResized;
  end;

implementation

uses
  System.Math,
  FMX.Forms;

{xIFDEF WINDOWS}
  {xDEFINE USE_ASM}
{xENDIF}

resourcestring
  SGIFDecodingError = 'Error when decoding GIF LZW data';
  SExceptMsg = 'Exception Message';
  SAllFilter = 'All Images';
  SUnknownFormat = 'Unknown and unsupported format';
  SErrorFreeImage = 'Error while freeing image. %s';
  SErrorCloneImage = 'Error while cloning image. %s';
  SErrorFlipImage = 'Error while flipping image. %s';
  SErrorMirrorImage = 'Error while mirroring image. %s';
  SErrorResizeImage = 'Error while resizing image.  %s';
  SErrorSwapImage = 'Error while swapping channels of image. %s';
  SFileFormatCanNotLoad = 'Image Format "%s" does not support loading images.';
  SFileFormatCanNotSave = 'Image Format "%s" does not support saving images.';
  SErrorNewImage = 'Error while creating image data with params: Width=%d Height=%d Format=%s.';
  SErrorConvertImage = 'Error while converting image to format "%s". %s';
  SImageInfo = 'Image @%p info: Width = %dpx, Height = %dpx, Format = %s, Size = %.0n %s, Bits @%p, Palette @%p.';
  SImageInfoInvalid = 'Access violation encountered when getting info on image at address %p.';
  SFileNotValid = 'File "%s" is not valid image in "%s" format.';
  SStreamNotValid = 'Stream %p does not contain valid image in "%s" format.';
  SMemoryNotValid = 'Memory %p (%d Bytes) does not contain valid image in "%s" format.';
  SErrorLoadingFile = 'Error while loading images from file "%s" (file format: %s).';
  SErrorLoadingStream = 'Error while loading images from stream %p (file format: %s).';
  SErrorLoadingMemory = 'Error while loading images from memory %p (%d Bytes) (file format: %s).';
  SErrorSavingFile = 'Error while saving images to file "%s" (file format: %s).';
  SErrorSavingStream = 'Error while saving images to stream %p (file format: %s).';
  SErrorSavingMemory = 'Error while saving images to memory %p (%d Bytes) (file format: %s).';
  SErrorFindColor = 'Error while finding color in palette @%p with %d entries.';
  SErrorGrayscalePalette = 'Error while filling grayscale palette @%p with %d entries.';
  SErrorCustomPalette = 'Error while filling custom palette @%p with %d entries.';
  SErrorSwapPalette = 'Error while swapping channels of palette @%p with %d entries.';
  SErrorReduceColors = 'Error while reducing number of colors of image to %d. %s';
  SErrorGenerateMipMaps = 'Error while generating %d mipmap levels for image %s';
  SImagesNotValid = 'One or more images are not valid.';
  SErrorCopyRect = 'Error while copying rect from image %s to image %s.';
  SErrorMapImage = 'Error while mapping image %s to palette.';
  SErrorFillRect = 'Error while filling rectangle X:%d Y:%d W:%d H:%d in image %s';
  SErrorSplitImage = 'Error while splitting image %s to %dx%d sized chunks.';
  SErrorMakePaletteForImages = 'Error while making %d color palette for %d images.';
  SErrorNewPalette = 'Error while creating new palette with %d entries';
  SErrorFreePalette = 'Error while freeing palette @%p';
  SErrorCopyPalette = 'Error while copying %d entries from palette @%p to @%p';
  SErrorReplaceColor = 'Error while replacing colors in rectangle X:%d Y:%d W:%d H:%d of image %s';
  SErrorRotateImage = 'Error while rotating image %s by %.2n degrees';
  SErrorStretchRect = 'Error while stretching rect from image %s to image %s.';
  SErrorEmptyStream = 'Input stream has no data. Check Position property.';
  SErrorInvalidInputImage = 'Invalid input image.';
  SErrorBadImage = 'Bad image detected.';

type
  TGIFVersion = (gv87, gv89);

{  TChar2 = array[0..1] of AnsiChar;
  TChar4 = array[0..3] of AnsiChar;
  TChar8 = array[0..7] of AnsiChar;
  TChar16 = array[0..15] of AnsiChar;
  TAnsiCharSet = set of AnsiChar;}

  TWordArray = array[0..MaxInt div 2 - 1] of Word;
  PWordArray = ^TWordArray;
  TLongIntArray = array[0..MaxInt div 8 - 1] of LongInt;
  PLongIntArray = ^TLongIntArray;
  TLongWordArray = array[0..MaxInt div 8 - 1] of LongWord;
  PLongWordArray = ^TLongWordArray;

  TWordRec = packed record
  case Integer of
  0: (WordValue: Word);
  1: (Low, High: Byte);
  end;
  PWordRec = ^TWordRec;
  TWordRecArray = array[0..MaxInt div 2 - 1] of TWordRec;
  PWordRecArray = ^TWordRecArray;

  TLongWordRec = packed record
  case Integer of
    0: (LongWordValue: LongWord);
    1: (Low, High: Word);
    { Array variants - Index 0 means lowest significant byte (word, ...).}
      2: (Words: array[0..1] of Word);
      3: (Bytes: array[0..3] of Byte);
  end;
  PLongWordRec = ^TLongWordRec;
  TLongWordRecArray = array[0..MaxInt div 8 - 1] of TLongWordRec;
  PLongWordRecArray = ^TLongWordRecArray;
  
  TImageDescriptor = packed record
    Left: Word;
    Top: Word;
    Width: Word;
    Height: Word;
    PackedFields: Byte;
  end;

const
  SGIFFormatName = 'Graphics Interchange Format';
  SGIFMasks      = '*.gif';
  GIFSupportedFormats: TImageFormats = [ifIndex8];
  GIFDefaultLoadAnimated = True;
//  GIFSignature: TChar3 = [Ord('G'), Ord('I'), Ord('F')];
//  GIFVersions: array[TGIFVersion] of TChar3 = ('87a', '89a');
  GIFDefaultDelay     = 65;
  GIFGlobalColorTable = $80;
  GIFColorResolution  = $70;
  GIFColorTableSorted = $08;
  GIFColorTableSize   = $07;
  GIFLocalColorTable  = $80;
  GIFInterlaced       = $40;
  GIFLocalTableSorted = $20;
  GIFPlainText: Byte               = $01;
  GIFGraphicControlExtension: Byte = $F9;
  GIFCommentExtension: Byte        = $FE;
  GIFApplicationExtension: Byte    = $FF;
  GIFImageDescriptor: Byte         = Ord(',');
  GIFExtensionIntroducer: Byte     = Ord('!');
  GIFTrailer: Byte                 = Ord(';');
  GIFBlockTerminator: Byte         = $00;
  GIFTransparent    = $01;
  GIFUserInput      = $02;
  GIFDisposalMethod = $1C;
  GIFAppLoopExtension   = 1;
  GIFAppBufferExtension = 2;
  GIFExtTypeGraphic     = $F9;
  GIFExtTypePlainText   = $01;
  GIFExtTypeApplication = $FF;
  GIFExtTypeComment     = $FE;

const
  CodeTableSize = 4096;
  HashTableSize = 17777;

type
  TGraphicControlExtension = packed record
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

  TReadContext = record
    Inx: Integer;
    Size: Integer;
    Buf: array [0..255 + 4] of Byte;
    CodeSize: Integer;
    ReadMask: Integer;
  end;
  PReadContext = ^TReadContext;

  TOutputContext = record
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

  EImagingError = class(Exception);
  { Raised when function receives bad image (not passed TestImage).}
  EImagingBadImage = class(Exception)
  public
    constructor Create;
  end;

constructor EImagingBadImage.Create;
begin
  inherited Create(SErrorBadImage);
end;

function GetExceptObject: Exception;
begin
  Result := Exception(ExceptObject);
  end;

procedure RaiseImaging(const Msg: string; const Args: array of const); overload;
var
  WholeMsg: string;
begin
  WholeMsg := Msg;
  if GetExceptObject <> nil then
  begin
    WholeMsg := WholeMsg + ' ' + SExceptMsg + ': ' +
      GetExceptObject.Message;
  end;
  raise EImagingError.CreateFmt(WholeMsg, Args);
end;

procedure RaiseImaging(const Msg: string); overload;
begin
  RaiseImaging(Msg, []);
end;

procedure ClipRectBounds(var X, Y, Width, Height: LongInt; const Clip: TRect);

  procedure ClipDim(var AStart, ALength: LongInt; ClipMin, ClipMax: LongInt);
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
  ReadCtxt: TReadContext;
  OutCtxt: TOutputContext;
  TableFull: Boolean;

  function ReadCode(var Context: TReadContext): Integer;
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

  procedure Output(Value: Byte; var Context: TOutputContext);
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
      RaiseImaging(SGIFDecodingError, []);
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
            RaiseImaging(SGIFDecodingError, []);
          OutCode^[OutCount] := Suffix^[CurCode];
          Inc(OutCount);
          CurCode := Prefix^[CurCode];
        end;

        FinalChar := CurCode and BitMask;
        OutCode^[OutCount] := FinalChar;
        Inc(OutCount);
        for I := OutCount - 1 downto 0 do
          Output(Byte(OutCode^[I]), OutCtxt);
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
      RaiseImaging(SGIFDecodingError, []);
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

function UpdateExceptMessage(E: Exception; const MsgToPrepend: string; const Args: array of const): Exception;
begin
  Result := E;
  E.Message := Format(MsgToPrepend, Args) +  ' ' + SExceptMsg + ': ' + E.Message
end;

function GetPixelSize(Format: TImageFormat; Width, Height: LongInt): LongInt;
begin
  case Format of
    ifIndex8:
    begin
      Result := Width * Height;
    end;
    ifA8R8G8B8:
    begin
      Result := Width * Height * 4;
    end
  else
    Result := 0;
  end;
end;

function TestImage(const Image: TImageData): Boolean;
begin
  try
    Result := (Image.Format <> ifUnknown) and (GetPixelSize(Image.Format, Image.Width, Image.Height) = Image.Size);
  except
    Result := False;
  end;
end;

procedure InitImage(var Image: TImageData);
begin
  FillChar(Image, SizeOf(Image), 0);
end;

procedure FreeImage(var Image: TImageData);
begin
  try
    if TestImage(Image) then
    begin
      FreeMemNil(Image.Bits);
      FreeMemNil(Image.Palette);
    end;
    InitImage(Image);
  except
    raise UpdateExceptMessage(GetExceptObject, SErrorFreeImage, []);
  end;
end;

function NewImage(Width, Height: LongInt; Format: TImageFormat; var Image: TImageData): Boolean;
begin
  Assert((Width > 0) and (Height >0));
  Assert(Format <> TImageFormat.ifUnknown);
  Result := False;
  FreeImage(Image);
  try
    Image.Width := Width;
    Image.Height := Height;
    // Select default data format if selected
    if (Format = ifUnknown) then
      Image.Format := TImageFormat.ifA8R8G8B8
    else
      Image.Format := Format;
    // Check image dimensions and calculate its size in bytes
    Image.Size := GetPixelSize(Format, Image.Width, Image.Height);
    if Image.Size = 0 then
    begin
      InitImage(Image);
      Exit;
    end;
    // Image bits are allocated and set to zeroes
    GetMem(Image.Bits, Image.Size);
    FillChar(Image.Bits^, Image.Size, 0);
    // Palette is allocated and set to zeroes
    if Image.Format = TImageFormat.ifIndex8 then
    begin
      GetMem(Image.Palette, 256 * SizeOf(TColor32Rec));
      FillChar(Image.Palette^, 256 * SizeOf(TColor32Rec), 0);
    end;
    Result := TestImage(Image);
  except
    on E: Exception do
    begin
      FreeMem(Image.Bits);
      FreeMem(Image.Palette);
      InitImage(Image);
      raise UpdateExceptMessage(E, SErrorNewImage, [Width, Height]);
    end;
  end;
end;

function CloneImage(const Image: TImageData; var Clone: TImageData): Boolean;
begin
  Result := False;
  if TestImage(Image) then
  try
    if TestImage(Clone) and (Image.Bits <> Clone.Bits) then
      FreeImage(Clone)
    else
      InitImage(Clone);

    Clone.Width := Image.Width;
    Clone.Height := Image.Height;
    Clone.Format := Image.Format;
    Clone.Size := Image.Size;

    if Image.Format = TImageFormat.ifIndex8 then
    begin
      GetMem(Clone.Palette, 256 * SizeOf(TColor32Rec));
      Move(Image.Palette^, Clone.Palette^, 256 * SizeOf(TColor32Rec));
    end;

    GetMem(Clone.Bits, Clone.Size);
    Move(Image.Bits^, Clone.Bits^, Clone.Size);
    Result := True;
  except
    raise UpdateExceptMessage(GetExceptObject, SErrorCloneImage, []);
  end;
end;

function Pow2Int(Exponent: LongInt): LongInt;
begin
  Result := 1 shl Exponent;
end;

procedure FillCustomPalette(Pal: PPalette32; Entries: LongInt; RBits, GBits, BBits: Byte; Alpha: Byte = $FF);
var
  I, TotalBits, MaxEntries: LongInt;
begin
  Assert(Pal <> nil);
  TotalBits := RBits + GBits + BBits;
  MaxEntries := Min(Pow2Int(TotalBits), Entries);
  FillChar(Pal^, Entries * SizeOf(TColor32Rec), 0);
  try
    for I := 0 to MaxEntries - 1 do
    with Pal[I] do
    begin
      A := Alpha;
      if RBits > 0 then
        R := ((I shr Max(0, GBits + BBits - 1)) and (1 shl RBits - 1)) * 255 div (1 shl RBits - 1);
      if GBits > 0 then
        G := ((I shr Max(0, BBits - 1)) and (1 shl GBits - 1)) * 255 div (1 shl GBits - 1);
      if BBits > 0 then
        B := ((I shr 0) and (1 shl BBits - 1)) * 255 div (1 shl BBits - 1);
    end;
  except
    RaiseImaging(SErrorCustomPalette, [Pal, Entries]);
  end;
end;

procedure FillMemoryLongWord(Data: Pointer; Size: LongInt; Value: LongWord);
{$IFDEF USE_ASM}
asm
  PUSH   EDI
  PUSH   EBX
  MOV    EBX, EDX
  MOV    EDI, EAX
  MOV    EAX, ECX
  MOV    ECX, EDX
  SHR    ECX, 2
  JZ     @Word
  REP    STOSD
@Word:
  MOV    ECX, EBX
  AND    ECX, 2
  JZ     @Byte
  MOV    [EDI], AX
  ADD    EDI, 2
@Byte:
  MOV    ECX, EBX
  AND    ECX, 1
  JZ     @Exit
  MOV    [EDI], AL
@Exit:
  POP    EBX
  POP    EDI
end;
{$ELSE}
var
  I: LongInt;
begin
  for I := 0 to Size div 4 - 1 do
    PLongWordArray(Data)[I] := Value;
  case Size mod 4 of
    1: PByteArray(Data)[Size - 1] := TLongWordRec(Value).Bytes[0];
    2: PWordArray(Data)[Size div 2] := TLongWordRec(Value).Words[0];
    3:
      begin
        PWordArray(Data)[Size div 2 - 1] := TLongWordRec(Value).Words[0];
        PByteArray(Data)[Size - 1] := TLongWordRec(Value).Bytes[0];
      end;
  end;
end;
{$ENDIF}

procedure FillMemoryByte(Data: Pointer; Size: LongInt; Value: Byte);
{$IFDEF USE_ASM}
asm
  PUSH   EDI
  MOV    EDI, EAX
  MOV    EAX, ECX
  MOV    AH, AL
  MOV    CX, AX
  SHL    EAX, 16
  MOV    AX, CX
  MOV    ECX, EDX
  SAR    ECX, 2
  JS     @Exit
  REP    STOSD
  MOV    ECX, EDX
  AND    ECX, 3
  REP    STOSB
  POP    EDI
@Exit:
end;
{$ELSE}
begin
  FillChar(Data^, Size, Value);
end;
{$ENDIF}

procedure FillMemoryWord(Data: Pointer; Size: LongInt; Value: Word);
{$IFDEF USE_ASM}
asm
  PUSH   EDI
  PUSH   EBX
  MOV    EBX, EDX
  MOV    EDI, EAX
  MOV    EAX, ECX
  MOV    CX, AX
  SHL    EAX, 16
  MOV    AX, CX
  MOV    ECX, EDX
  SHR    ECX, 2
  JZ     @Word
  REP    STOSD
@Word:
  MOV    ECX, EBX
  AND    ECX, 2
  JZ     @Byte
  MOV    [EDI], AX
  ADD    EDI, 2
@Byte:
  MOV    ECX, EBX
  AND    ECX, 1
  JZ     @Exit
  MOV    [EDI], AL
@Exit:
  POP    EBX
  POP    EDI
end;
{$ELSE}
var
  I, V: LongWord;
begin
  V := Value * $10000 + Value;
  for I := 0 to Size div 4 - 1 do
    PLongWordArray(Data)[I] := V;
  case Size mod 4 of
    1: PByteArray(Data)[Size - 1] := Lo(Value);
    2: PWordArray(Data)[Size div 2] := Value;
    3:
      begin
        PWordArray(Data)[Size  div 2 - 1] := Value;
        PByteArray(Data)[Size - 1] := Lo(Value);
      end;
  end;
end;
{$ENDIF}

procedure CopyPixel(Src, Dest: Pointer; BytesPerPixel: LongInt);
begin
  case BytesPerPixel of
    1: PByte(Dest)^ := PByte(Src)^;
    2: PWord(Dest)^ := PWord(Src)^;
    3: PColor24Rec(Dest)^ := PColor24Rec(Src)^;
    4: PLongWord(Dest)^ := PLongWord(Src)^;
  end;
end;

procedure ClipCopyBounds(var SrcX, SrcY, Width, Height, DstX, DstY: LongInt; SrcImageWidth, SrcImageHeight: LongInt; const DstClip: TRect);

  procedure ClipDim(var SrcPos, DstPos, Size: LongInt; SrcClipMax, DstClipMin, DstClipMax: LongInt);
  var
    OldDstPos: LongInt;
    Diff: LongInt;
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

procedure ConvertToPixel32(SrcPix: PByte; DestPix: PColor32Rec; const SrcFormat: TImageFormat; SrcPalette: PPalette32);
begin
  case SrcFormat of
    ifIndex8:
    begin
      DestPix^ := SrcPalette[SrcPix^];
    end;
  else
    DestPix^ := PColor32Rec(SrcPix)^;
  end;
end;

procedure CopyFrameTransparent32(const Image, Frame: TImageData; Left, Top: Integer);
var
  X, Y: Integer;
  Src: PByte;
  Dst: PColor32;
begin
  Src := Frame.Bits;
  // Copy all pixels from frame to log screen but ignore the transparent ones
  for Y := 0 to Frame.Height - 1 do
  begin
    Dst := @PColor32RecArray(Image.Bits)[(Top + Y) * Image.Width + Left];
    for X := 0 to Frame.Width - 1 do
    begin
      if (Frame.Palette[Src^].A <> 0) then
        Dst^ := Frame.Palette[Src^].Color;
      Inc(Src);
      Inc(Dst);
    end;
  end;
end;

procedure CopyRectToBitmap(const Image: TImageData; Bitmap: TBitmap; SrcX, SrcY, Width, Height, DstX, DstY: Integer);
var
  X, Y, Bpp, SrcWidthBytes, MoveBytes: Integer;
  DstPtr: PColor32Rec;
  SrcPtr: PByte;
  Data : TBitmapData;
begin
  Assert(TestImage(Image) and not Bitmap.IsEmpty);

  ClipCopyBounds(SrcX, SrcY, Width, Height, DstX, DstY, Image.Width, Image.Height, Rect(0, 0, Bitmap.Width, Bitmap.Height));
  if Image.Format = TImageFormat.ifIndex8 then
  begin
    Bpp := 1;
  end
  else begin
    Bpp := 4;
  end;
  SrcWidthBytes := Image.Width * Bpp;
  MoveBytes := Width * Bpp;
  SrcPtr := @PByteArray(Image.Bits)[SrcY * SrcWidthBytes + SrcX * Bpp];
  Bitmap.Map(TMapAccess.ReadWrite, Data);
  try
    for Y := 0 to Height - 1 do
    begin
      DstPtr := PColor32Rec(Data.GetScanLine(Y));
      if Image.Format = ifA8R8G8B8 then
      begin
        Move(SrcPtr^, DstPtr^, MoveBytes);
        Inc(SrcPtr, MoveBytes);
      end
      else begin
        for X := 0 to Width - 1 do
        begin
          ConvertToPixel32(SrcPtr, DstPtr, Image.Format, Image.Palette);
          Inc(DstPtr);
          Inc(SrcPtr, Bpp);
        end;
      end;
    end;
  finally
    Bitmap.Unmap(Data);
  end;
end;

procedure ConvertImageDataToBitmap(const Image: TImageData; Bitmap: TBitmap);
begin
  Assert(TestImage(Image));
  Bitmap.SetSize(Image.Width, Image.Height);
  CopyRectToBitmap(Image, Bitmap, 0, 0, Image.Width, Image.Height, 0, 0);
end;

function FillRect(var Image: TImageData; X, Y, Width, Height: LongInt; FillColor: Pointer): Boolean;
var
  I, J, ImageWidthBytes, RectWidthBytes, Bpp: Longint;
  LinePointer, PixPointer: PByte;
begin
  Result := False;
  if TestImage(Image) then
  try
    ClipRectBounds(X, Y, Width, Height, Rect(0, 0, Image.Width, Image.Height));

    if (Width > 0) and (Height > 0) then
    begin
      if Image.Format = TImageFormat.ifIndex8 then
      begin
        Bpp := 1;
      end
      else begin
        Bpp := 4;
      end;
      ImageWidthBytes := Image.Width * Bpp;
      RectWidthBytes := Width * Bpp;
      LinePointer := @PByteArray(Image.Bits)[Y * ImageWidthBytes + X * Bpp];

      for I := 0 to Height - 1 do
      begin
        case Bpp of
          1: FillMemoryByte(LinePointer, RectWidthBytes, PByte(FillColor)^);
          2: FillMemoryWord(LinePointer, RectWidthBytes, PWord(FillColor)^);
          4: FillMemoryLongWord(LinePointer, RectWidthBytes, PLongWord(FillColor)^);
        else
          PixPointer := LinePointer;
          for J := 0 to Width - 1 do
          begin
            CopyPixel(FillColor, PixPointer, Bpp);
            Inc(PixPointer, Bpp);
          end;
        end;
        Inc(LinePointer, ImageWidthBytes);
      end;

    end;

    Result := True;
  except
    RaiseImaging(SErrorFillRect, [X, Y, Width, Height]);
  end;
end;

const
  DefaultBufferSize = 16 * 1024;

type
  { Based on TaaBufferedStream
    Copyright (c) Julian M Bucknall 1997, 1999 }
  TBufferedStream = class
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
  InitImage(FCachedImage);
  FPlayer := nil;
  FCachedIndex := -1;
end;

destructor TGIFData.Destroy;
begin
  InternalClear;
  FreeAndNil(FStream);
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
  HasGlobalPal: Boolean;
  GlobalPalLength: Integer;
  GlobalPal: TPalette32Size256;
  ScreenWidth, ScreenHeight, I : Integer;
  BlockID: Byte;
  HasGraphicExt: Boolean;
  GraphicExt: TGraphicControlExtension;
  AppRead: Boolean;

  function ReadBlockID: Byte;
  begin
    Result := GIFTrailer;
    if Buffer.Read(Result, SizeOf(Result)) < SizeOf(Result) then
    begin
      Result := GIFTrailer;
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

    while BlockID = GIFExtensionIntroducer do
    begin
      Buffer.Read(ExtType, SizeOf(ExtType));

      while ExtType in [GIFGraphicControlExtension, GIFCommentExtension, GIFApplicationExtension, GIFPlainText] do
      begin
        if ExtType = GIFGraphicControlExtension then
        begin
          HasGraphicExt := True;
          Buffer.Read(GraphicExt, SizeOf(GraphicExt));
        end
        else if (ExtType = GIFApplicationExtension) and not AppRead then
        begin
          Buffer.Read(BlockSize, SizeOf(BlockSize));
          if BlockSize >= SizeOf(AppRec) then
          begin
            Buffer.Read(AppRec, SizeOf(AppRec));
            if ((TEncoding.ANSI.GetString(AppRec.Identifier) = 'NETSCAPE') and (TEncoding.ANSI.GetString(AppRec.Authentication) = '2.0')) or ((TEncoding.ANSI.GetString(AppRec.Identifier) = 'ANIMEXTS') and (TEncoding.ANSI.GetString(AppRec.Authentication) = '1.0')) then
            begin
              Buffer.Read(BlockSize, SizeOf(BlockSize));
              while BlockSize <> 0 do
              begin
                BlockType := ReadBlockID;
                Dec(BlockSize);

                case BlockType of
                  GIFAppLoopExtension:
                    if (BlockSize >= SizeOf(LoopCount)) then
                    begin
                      Buffer.Read(LoopCount, SizeOf(LoopCount));
                      Dec(BlockSize, SizeOf(LoopCount));
                      if LoopCount > 0 then Inc(LoopCount);
                      //FMetadata.AddMetaItem(SMetaAnimationLoops, LoopCount);
                    end;
                  GIFAppBufferExtension:
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
        else if ExtType in [GIFCommentExtension, GIFApplicationExtension, GIFPlainText] then
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
    ImageDesc: TImageDescriptor;
    Interlaced: Boolean;
    I, LocalPalLength: Integer;
    LocalPal: TPalette32Size256;
    LZWStream: TMemoryStream;
    NewFrame : TGIFFrame;

  begin

    NewFrame := TGIFFrame.Create;
    FFrames.Add(NewFrame);
    FillChar(LocalPal, SizeOf(LocalPal), 0);
    Buffer.Read(ImageDesc, SizeOf(ImageDesc));
    NewFrame.FHasLocalPal := (ImageDesc.PackedFields and GIFLocalColorTable) = GIFLocalColorTable;
    Interlaced := (ImageDesc.PackedFields and GIFInterlaced) = GIFInterlaced;
    LocalPalLength := ImageDesc.PackedFields and GIFColorTableSize;
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
    NewFrame.FBackIndex := FHeader.BackgroundColorIndex;
    NewImage(ImageDesc.Width, ImageDesc.Height, ifIndex8, NewFrame.FData);
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
    else if HasGlobalPal then
    begin
      Move(GlobalPal, NewFrame.FData.Palette^, SizeOf(GlobalPal))
    end
    else begin
      FillCustomPalette(NewFrame.FData.Palette, GlobalPalLength, 3, 3, 2);
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
      NewFrame.FHasTransparency := (GraphicExt.PackedFields and GIFTransparent) = GIFTransparent;
      NewFrame.FDisposal := TGIFDisposalMethod((GraphicExt.PackedFields and GIFDisposalMethod) shr 2);
      if NewFrame.FHasTransparency then
      begin
        NewFrame.FTransIndex := GraphicExt.TransparentColorIndex;
        NewFrame.FData.Palette[NewFrame.FTransIndex].A := 0;
      end;
      NewFrame.FDelay := Integer(GraphicExt.DelayTime * 10);
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
    FillChar(GlobalPal, SizeOf(GlobalPal), 0);

    with Buffer do
    begin
      Read(FHeader, SizeOf(FHeader));
      ScreenWidth := FHeader.ScreenWidth;
      ScreenHeight := FHeader.ScreenHeight;
      HasGlobalPal := FHeader.PackedFields and GIFGlobalColorTable = GIFGlobalColorTable;
      GlobalPalLength := FHeader.PackedFields and GIFColorTableSize;
      GlobalPalLength := 1 shl (GlobalPalLength + 1);
      if HasGlobalPal then
      begin
        for I := 0 to GlobalPalLength - 1 do
        begin
          GlobalPal[I].A := 255;
          Read(GlobalPal[I].R, SizeOf(GlobalPal[I].R));
          Read(GlobalPal[I].G, SizeOf(GlobalPal[I].G));
          Read(GlobalPal[I].B, SizeOf(GlobalPal[I].B));
        end;
      end;
      BlockID := ReadBlockID;
      while BlockID <> GIFTrailer do
      begin
        while not (BlockID in [GIFTrailer, GIFExtensionIntroducer, GIFImageDescriptor]) do
        begin
          BlockID := ReadBlockID;
        end;
        ReadExtensions;
        if BlockID = GIFImageDescriptor then
        begin
          ReadFrame;
        end;
        BlockID := ReadBlockID;
        if not (BlockID in [GIFExtensionIntroducer, GIFTrailer, GIFImageDescriptor]) then
        begin
          BlockID := GIFTrailer;
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

procedure TGIFData.DoChange;
begin
  if Assigned(FOnChange) then
  begin
    FOnChange(Self);
  end;
end;

procedure TGIFData.InternalClear;
begin
  Stop;
  FStream.Clear;
  FreeImage(FCachedImage);
  FCachedIndex := -1;
  FFrames.Clear;
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

procedure TGIFData.InternalRender(AIndex: Integer; var AFrame: TImageData);
var
  I, First, Last: Integer;
  UseCache: Boolean;
  BGColor: TColor32;
begin
  Last := AIndex;
  First := Max(0, Last);
  UseCache := TestImage(FCachedImage) and (FCachedIndex = AIndex - 1) and (FCachedIndex >= 0) and (FFrames[FCachedIndex].Disposal <> dmRestorePrevious);
  if UseCache then
  begin
    CloneImage(FCachedImage, AFrame)
  end
  else begin
    FreeImage(FCachedImage);
  end;
  BGColor := FFrames[AIndex].Data.Palette[FFrames[AIndex].BackIndex].Color;
  if not UseCache then
  begin
    if FFrames[AIndex].HasTransparency then
    begin
      BGColor := $00000000;
    end;
    FillMemoryLongWord(AFrame.Bits, AFrame.Size, BGColor);
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
          CopyFrameTransparent32(AFrame, FFrames[I].Data, FFrames[I].Left, FFrames[I].Top);
        end;
        dmRestoreBackground:
        begin
          if (I > First) then
          begin
            FillRect(AFrame, FFrames[I].Left, FFrames[I].Top, FFrames[I].Width, FFrames[I].Height, @BGColor);
          end;
        end;
        dmRestorePrevious:;
      end;
    end;
  end
  else if FFrames[FCachedIndex].Disposal = dmRestoreBackground then
  begin
    if FFrames[FCachedIndex].HasTransparency then
    begin
      BGColor := $00000000;
    end;
    FillRect(AFrame, FFrames[FCachedIndex].Left, FFrames[FCachedIndex].Top, FFrames[FCachedIndex].Width, FFrames[FCachedIndex].Height, @BGColor);
  end;
  CopyFrameTransparent32(AFrame, FFrames[AIndex].Data, FFrames[AIndex].Left, FFrames[AIndex].Top);
  CloneImage(AFrame, FCachedImage);
  FCachedIndex := AIndex;
end;

procedure TGIFData.Render(ABitmap : TBitmap; AFrameIndex : Integer);
var
  AniFrame : TImageData;
begin
  if ABitmap.IsEmpty then
  begin
    ABitmap.SetSize(Header.ScreenWidth, Header.ScreenHeight);
    ABitmap.Clear(0);
  end;
  NewImage(Header.ScreenWidth, Header.ScreenHeight, ifA8R8G8B8, AniFrame);
  try
    InternalRender(AFrameIndex, AniFrame);
    ConvertImageDataToBitmap(AniFrame, ABitmap);
  finally
    FreeImage(AniFrame);
  end;
end;

procedure TGIFData.DoPlayStart;
begin
end;

procedure TGIFData.DoPlayStop;
begin
  FPlayer := nil;
end;

procedure TGIFData.Stop;
begin
  if Assigned(FPlayer) then
  begin
    FPlayer.Cancel;
    while Assigned(FPlayer) do
    begin
      Application.ProcessMessages;
    end;
  end;
end;

procedure TGIFData.Play(ABitmap : TBitmap);
begin

  if (FrameCount = 0) then Exit;
  
  if Assigned(FPlayer) then
  begin
  FPlayer.Cancel;
  end;
  
  FPlayer := TTask.Run
  (
    procedure()
    var
      FrameIndex : Integer;
      AniFrame   : TImageData;
    begin
      TThread.Synchronize
      (nil,
        procedure()
        begin
          DoPlayStart;
        end
  );
      FrameIndex := 0;
      while not (FPlayer.Status = TTaskStatus.Canceled) do
      begin
        NewImage(Header.ScreenWidth, Header.ScreenHeight, ifA8R8G8B8, AniFrame);
        try
          InternalRender(FrameIndex, AniFrame);
          TThread.Synchronize
          (nil,
            procedure()
            begin
              ConvertImageDataToBitmap(AniFrame, ABitmap);
            end
          );
        finally
          FreeImage(AniFrame);
        end;

        if not (FPlayer.Status = TTaskStatus.Canceled) then
        begin
          Sleep(FFrames[FrameIndex].Delay);
        end;

        Inc(FrameIndex);
        if FrameIndex > FrameCount - 1 then
        begin
          FrameIndex := 0;
        end;
      end;
      TThread.Synchronize
      (nil,
         procedure()
         begin
           DoPlayStop;
         end
      );

     end
   );

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

procedure TGIFData.SaveToStream(AStream : TStream);
begin
  FStream.SaveToStream(AStream);
end;

procedure TGIFData.SaveToFile(const AFilename : String);
begin
  FStream.SaveToFile(AFilename);
end;

{ **************************************************************************** }
{ TGIFImage }
{ **************************************************************************** }

constructor TGIFImage.Create(AOwner: TComponent);
begin
  inherited;
  FGIFData := TGIFData.Create;
  FBitmap := TBitmap.Create;
  FBitmap.OnChange := DoBitmapChanged;
  FWrapMode := TImageWrapMode.Fit;
  SetAcceptsControls(False);
  FScaleChangedId := TMessageManager.DefaultManager.SubscribeToMessage(TScaleChangedMessage, ScaleChangedHandler);
end;

destructor TGIFImage.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TScaleChangedMessage, FScaleChangedId);
  FreeAndNil(FGIFData);
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TGIFImage.DoChanged;
begin
  Repaint;
  UpdateEffects;
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
  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.DrawDashRect(R, 0, 0, AllCorners, AbsoluteOpacity, $A0909090);
  end;
  DrawBitmap(Canvas, LocalRect, FBitmap, AbsoluteOpacity);
end;

procedure TGIFImage.DoBitmapChanged(Sender : TObject);
begin
  Repaint;
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

procedure TGIFImage.Loaded;
begin
  inherited;
  if not FGIFData.IsEmpty then
  begin
    if csDesigning in ComponentState then
    begin
      FGIFData.Render(FBitmap, 0);
    end
    else begin
      FGIFData.Play(FBitmap);
    end;
  end;
end;

end.