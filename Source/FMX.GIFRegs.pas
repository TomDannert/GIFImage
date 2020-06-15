{ *****************************************************************************
                            GIF for Firemonkey
            Copyright (C) 2020 by Thomas Dannert <thomas@dannert.com>
                         https://github.com/TomDannert
  *****************************************************************************
  GIF for Firemonkey is free software: you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License version 3as
  published by the Free Software Foundation and appearing in the included file.
  GIF for Firemonkey is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
  for more details.
  **************************************************************************** }

unit FMX.GIFRegs;

interface

uses
  System.Classes,
  System.SysUtils,
  DesignEditors,
  DesignIntf,
  DesignMenus,
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.Generics.Collections,
  System.TypInfo,
  FMX.Types,
  FMX.Graphics,
  FMX.Ani,
  FMX.GIFImage;

type
  TGIFDataProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    function GetAttributes: TPropertyAttributes; override;
  end;

resourcestring
  GIF_Category = 'GIF';

procedure Register;

implementation

uses
  FMX.Dialogs;

{ **************************************************************************** }
{ TGIFDataProperty }
{ **************************************************************************** }

function TGIFDataProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties] - [paReadOnly];
end;

procedure TGIFDataProperty.Edit;
var
  OpenDialog : TOpenDialog;
  GIFData: TGIFData;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.Filter := 'GIF Files (*.GIF)|*.GIF|All Files (*.*)|*.*';
    OpenDialog.DefaultExt := 'GIF';
    if OpenDialog.Execute then
    begin
      GIFData := TGIFData(GetOrdValue);
      GIFData.LoadFromFile(OpenDialog.Filename);
      Modified;
    end;
  finally
    OpenDialog.Free;
  end;
end;

function TGIFDataProperty.GetValue: string;
var
  GIFData: TGIFData;
begin
  GIFData := TGIFData(GetOrdValue);
  if (GIFData = nil) or GIFData.IsEmpty then
    Result := '(empty)' else
    Result := Format('%d x %d (%d Frames)', [GIFData.Header.ScreenWidth, GIFData.Header.ScreenHeight, GIFData.FrameCount]);
end;

procedure TGIFDataProperty.SetValue(const Value: string);
var
  GIFData: TGIFData;
begin
  inherited;
  if Value = '' then
  begin
    GIFData := TGIFData(GetOrdValue);
    GIFData.Clear;
  end;
end;

procedure Register;
begin
  RegisterClass(TGIFData);
  RegisterPropertyEditor(TypeInfo(TGIFData), nil, '', TGIFDataProperty);
  RegisterComponents(GIF_Category, [TGIFImage]);
end;

end.
