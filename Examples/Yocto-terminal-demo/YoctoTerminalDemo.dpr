program YoctoTerminalDemo;

uses
  Forms,
  dialogs,
  yocto_api,
  main in 'main.pas' {Form1},
  setup in 'setup.pas' {Form2};

{$R *.RES}
var
  errmsg :string;
begin

  if (yregisterhub('usb',errmsg) <> YAPI_SUCCESS) then
    messagedlg(errmsg,mterror,[mbok],0)
  else
   begin
     Application.Initialize;
     Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
   end; 
end.
