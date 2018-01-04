unit setup;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, yocto_api,yocto_serialPort;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    baudrate: TComboBox;
    Label2: TLabel;
    encoding: TComboBox;
    Label3: TLabel;
    flowcontrol: TComboBox;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure baudrateChange(Sender: TObject);
  private
    { Private declarations }
    current:TYSerialPort ;
    previousmode:string;
  public
    procedure showit(f:TYSerialPort);
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

 procedure TForm2.showit(f:TYSerialPort);
 var i:integeR;
  config:string;
   begin
    if f<>nil then
     begin
       current:=f;
       config:= current.get_serialMode();
       for i:=0 to baudrate.items.count do
         if pos(baudrate.items[i],config)>0 then baudrate.itemIndex:=i;
       for i:=0 to encoding.items.count do
         if pos(encoding.items[i],config)>0 then encoding.itemIndex:=i;
       config:=f.get_serialMode();
        flowcontrol.itemIndex:=0;
       for i:=0 to  flowcontrol.items.count do
          if pos( flowcontrol.items[i],config)>0 then  flowcontrol.itemIndex:=i;
      previousmode:=f.get_serialMode();
       showModal();
     end;
   end;

{$R *.DFM}

procedure TForm2.Button2Click(Sender: TObject);
begin
 close();
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  current.set_serialMode(previousmode);
  close();
end;

procedure TForm2.Button3Click(Sender: TObject);
 var
  m:TYmodule;
begin
  m:=current.get_Module();
  m.Savetoflash();

end;

procedure TForm2.baudrateChange(Sender: TObject);
var
  value : string;
begin
  value := baudrate.items[baudrate.itemIndex]+','+encoding.items[encoding.itemIndex];
  if (flowcontrol.itemindex>0)  then
  value := value+','+ flowcontrol.items[flowcontrol.itemIndex];
  current.set_serialMode(value);
end;

end.
