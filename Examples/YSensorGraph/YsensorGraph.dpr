program YsensorGraph;

uses
  Forms, dialogs,  Yocto_api,
  main in 'main.pas' {Form1};

{$R *.RES}

var
  errmsg :string;
begin
  if  Yregisterhub('usb',errmsg) =YAPI_SUCCESS then
     begin
      Application.Initialize;
      Application.Title := 'YoctoGraph';
      Application.CreateForm(TForm1, Form1);
      Application.Run;
     end
     else messagedlg(errmsg,mterror,[mbok],0);
end.
