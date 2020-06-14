unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Permissions,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Objects, System.Actions,
  FMX.ActnList, FMX.StdActns, FMX.MediaLibrary.Actions, FMX.Layouts, FMX.Effects,
  FMX.Filter.Effects, FMX.Filter, FMX.Ani, FMX.Graphics,
  FMX.Controls.Presentation, FMX.ListBox, FMX.GIFImage;

type
  TFilterClass = class of TFilter;

  TBaseMainForm = class(TForm)
    ButtonPlay: TButton;
    ButtonClear: TButton;
    ToolBarBottom: TToolBar;
    ActionList: TActionList;
    ActionTakePhotoFromLibrary: TTakePhotoFromLibraryAction;
    ActionTakePhotoFromCamera: TTakePhotoFromCameraAction;
    ButtonSendImage: TButton;
    ActionShowShareSheet: TShowShareSheetAction;
    ButtonTakePhotoFromLibrary: TButton;
    ToolBarTop: TToolBar;
    ActionClear: TAction;
    RemoveBtnAnimation: TFloatAnimation;
    WrapModeComboBox: TComboBox;
    ButtonStop: TButton;
    GIFImage: TGIFImage;
    ActionPlay: TAction;
    ActionStop: TAction;
    procedure ActionTakePhotoFromLibraryDidFinishTaking(Image: TBitmap);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure ActionShowShareSheetBeforeExecute(Sender: TObject);
    procedure ActionResetEffectExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure WrapModeComboBoxChange(Sender: TObject);
    procedure ButtonTakePhotoFromLibraryClick(Sender: TObject);
    procedure ButtonPlayClick(Sender: TObject);
    procedure ActionPlayExecute(Sender: TObject);
    procedure ActionStopExecute(Sender: TObject);
    procedure ActionClearExecute(Sender: TObject);
  private
    FEffect: TFilter;
    FRawBitmap: TBitmap;
    FPermissionCamera,
    FPermissionReadExternalStorage,
    FPermissionWriteExternalStorage: string;
    procedure DisplayRationale(Sender: TObject; const APermissions: TArray<string>; const APostRationaleProc: TProc);
    procedure LoadPicturePermissionRequestResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
    procedure TakePicturePermissionRequestResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  BaseMainForm: TBaseMainForm;

implementation

uses
{$IFDEF ANDROID}
  Androidapi.Helpers,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
{$ENDIF}
  FMX.Colors,
  FMX.DialogService;

{$R *.fmx}

constructor TBaseMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRawBitmap := TBitmap.Create(0, 0);
{$IFDEF ANDROID}
  FPermissionCamera := JStringToString(TJManifest_permission.JavaClass.CAMERA);
  FPermissionReadExternalStorage := JStringToString(TJManifest_permission.JavaClass.READ_EXTERNAL_STORAGE);
  FPermissionWriteExternalStorage := JStringToString(TJManifest_permission.JavaClass.WRITE_EXTERNAL_STORAGE);
{$ENDIF}
  Fill.Kind := TBrushKind.Bitmap;
  MakeChessBoardBrush(Fill.Bitmap, 20);
end;

destructor TBaseMainForm.Destroy;
begin
  FreeAndNil(FRawBitmap);
  inherited Destroy;
end;

// Optional rationale display routine to display permission requirement rationale to the user
procedure TBaseMainForm.DisplayRationale(Sender: TObject; const APermissions: TArray<string>; const APostRationaleProc: TProc);
var
  I: Integer;
  RationaleMsg: string;
begin
  for I := 0 to High(APermissions) do
  begin
    if APermissions[I] = FPermissionCamera then
      RationaleMsg := RationaleMsg + 'The app needs to access the camera to take a photo' + SLineBreak + SLineBreak
    else if APermissions[I] = FPermissionReadExternalStorage then
      RationaleMsg := RationaleMsg + 'The app needs to load photo files from your device';
  end;

  // Show an explanation to the user *asynchronously* - don't block this thread waiting for the user's response!
  // After the user sees the explanation, invoke the post-rationale routine to request the permissions
  TDialogService.ShowMessage(RationaleMsg,
    procedure(const AResult: TModalResult)
    begin
      APostRationaleProc;
    end)
end;

procedure TBaseMainForm.ActionClearExecute(Sender: TObject);
begin
  RemoveBtnAnimation.Start;
  GIFImage.GIFData.Clear;
end;

procedure TBaseMainForm.ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  ActionPlay.Enabled  := not GIFImage.IsEmpty and not GIFImage.IsPlaying;
  ActionStop.Enabled  := GIFImage.IsPlaying;
  ActionClear.Enabled := not GIFImage.IsEmpty;
end;

procedure TBaseMainForm.ActionPlayExecute(Sender: TObject);
begin
  GIFImage.Play;
end;

procedure TBaseMainForm.ActionResetEffectExecute(Sender: TObject);
begin
  FreeAndNil(FEffect);
  //ImageContainer.Bitmap.Assign(FRawBitmap);
end;

procedure TBaseMainForm.ActionShowShareSheetBeforeExecute(Sender: TObject);
begin
  //ActionShowShareSheet.Bitmap := ImageContainer.Bitmap;
end;

procedure TBaseMainForm.ActionStopExecute(Sender: TObject);
begin
  GIFImage.Stop;
end;

procedure TBaseMainForm.ActionTakePhotoFromLibraryDidFinishTaking(Image: TBitmap);
var
  ScaleFactor: Single;
begin
  if Image.Width > 1024 then
  begin
    ScaleFactor := Image.Width / 1024;
    Image.Resize(Round(Image.Width / ScaleFactor), Round(Image.Height / ScaleFactor));
  end;
  FRawBitmap.Assign(Image);
  WrapModeComboBox.ItemIndex := 0;
end;

procedure TBaseMainForm.ButtonPlayClick(Sender: TObject);
begin
  PermissionsService.RequestPermissions([FPermissionCamera, FPermissionReadExternalStorage, FPermissionWriteExternalStorage], TakePicturePermissionRequestResult, DisplayRationale);
end;

procedure TBaseMainForm.ButtonTakePhotoFromLibraryClick(Sender: TObject);
begin
  PermissionsService.RequestPermissions([FPermissionReadExternalStorage, FPermissionWriteExternalStorage], LoadPicturePermissionRequestResult, DisplayRationale);
end;

procedure TBaseMainForm.WrapModeComboBoxChange(Sender: TObject);
begin
 // if WrapModeComboBox.Selected.Text = 'None' then
//    ActionResetEffect.Execute;
end;

procedure TBaseMainForm.FormCreate(Sender: TObject);
begin
  WrapModeComboBox.Items.Add('Fit');
  WrapModeComboBox.Items.Add('Stretch');
  WrapModeComboBox.Items.Add('Center');
end;

procedure TBaseMainForm.LoadPicturePermissionRequestResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
begin
  // 2 permissions involved: READ_EXTERNAL_STORAGE, WRITE_EXTERNAL_STORAGE
  if (Length(AGrantResults) = 2) and
     (AGrantResults[0] = TPermissionStatus.Granted) and
     (AGrantResults[1] = TPermissionStatus.Granted) then
    ActionTakePhotoFromLibrary.Execute
  else
    TDialogService.ShowMessage('Cannot do photo editing because the required permissions are not granted');
end;

procedure TBaseMainForm.TakePicturePermissionRequestResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
begin
  // 3 permissions involved: CAMERA, READ_EXTERNAL_STORAGE, WRITE_EXTERNAL_STORAGE
  if (Length(AGrantResults) = 3) and
     (AGrantResults[0] = TPermissionStatus.Granted) and
     (AGrantResults[1] = TPermissionStatus.Granted) and
     (AGrantResults[2] = TPermissionStatus.Granted) then
    ActionTakePhotoFromCamera.Execute
  else
    TDialogService.ShowMessage('Cannot take picture because the required permissions are not granted');
end;

end.
