program GIFImageDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainFrm in 'MainFrm.pas' {BaseMainForm};

begin
  Application.Initialize;
  Application.CreateForm(TBaseMainForm, BaseMainForm);
  Application.Run;
end.

