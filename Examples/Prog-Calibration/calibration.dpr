program calibration;

uses
  Forms,   dialogs,
  yocto_api,
  mainform in 'mainform.pas' {Form1};

{$R *.RES}
var errsmg :string;

begin

  if (yregisterhub('usb',errsmg)<>YAPI_SUCCESS) then
   begin
   ShowMessage('unable to initialise Yocto-API: '+errsmg	);
   end
   else
   begin
    Application.Initialize;
    Application.Title := 'Yoctopuce devices calibration example';
  Application.CreateForm(TForm1, Form1);
    Application.Run;
   end;
end.
