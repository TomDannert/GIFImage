unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Permissions,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Objects, System.Actions,
  FMX.ActnList, FMX.StdActns, FMX.MediaLibrary.Actions, FMX.Layouts, FMX.Effects,
  FMX.Filter.Effects, FMX.Filter, FMX.Ani, FMX.Graphics,
  FMX.Controls.Presentation, FMX.ListBox, FMX.GIFImage;

type
  TBaseMainForm = class(TForm)
    ButtonPlay: TButton;
    ButtonClear: TButton;
    ToolBarBottom: TToolBar;
    ActionList: TActionList;
    ButtonOpen: TButton;
    ToolBarTop: TToolBar;
    ActionClear: TAction;
    RemoveBtnAnimation: TFloatAnimation;
    WrapModeComboBox: TComboBox;
    ButtonStop: TButton;
    GIFImage: TGIFImage;
    ActionPlay: TAction;
    ActionStop: TAction;
    OpenDialog: TOpenDialog;
    ActionOpen: TAction;
    procedure WrapModeComboBoxChange(Sender: TObject);
    procedure ActionPlayExecute(Sender: TObject);
    procedure ActionStopExecute(Sender: TObject);
    procedure ActionClearExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure ActionOpenExecute(Sender: TObject);
  private
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  BaseMainForm: TBaseMainForm;

implementation

uses
  FMX.Colors,
  FMX.DialogService;

{$R *.fmx}

constructor TBaseMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Fill.Kind := TBrushKind.Bitmap;
  MakeChessBoardBrush(Fill.Bitmap, 8);
end;

destructor TBaseMainForm.Destroy;
begin
  inherited Destroy;
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

procedure TBaseMainForm.ActionOpenExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    GIFImage.LoadFromFile(OpenDialog.Filename);
    GIFImage.Play;
  end;
end;

procedure TBaseMainForm.ActionPlayExecute(Sender: TObject);
begin
  GIFImage.Play;
end;

procedure TBaseMainForm.ActionStopExecute(Sender: TObject);
begin
  GIFImage.Stop;
end;

procedure TBaseMainForm.WrapModeComboBoxChange(Sender: TObject);
begin
  GIFImage.WrapMode := TImageWrapMode(WrapModeComboBox.ItemIndex);
end;

end.
