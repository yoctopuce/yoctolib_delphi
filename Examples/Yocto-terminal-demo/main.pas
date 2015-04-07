unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls,Yocto_api,Yocto_SerialPort, ExtCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    StatusBar1: TStatusBar;
    ComboBox1: TComboBox;
    setupBtn: TButton;
    Timer1: TTimer;
    Timer2: TTimer;
    Label1: TLabel;
    Edit1: TEdit;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Memo1KeyPress(Sender: TObject; var Key: Char);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure setupBtnClick(Sender: TObject);
  private
    { Private declarations }
     currentPort : TYSerialPort;
        tosend:string;
     procedure refreshCurrentPort();


  public
    { Public declarations }
     procedure arrival(m:TYModule);
     procedure removal(m:TYModule);
      procedure BufferStateChange();
  end;

var
  Form1: TForm1;

implementation

uses setup;

{$R *.DFM}

procedure  deviceArrival(m:TYModule);
 begin
  form1.arrival(m);
 end;

procedure  deviceRemoval(m:TYModule);
 begin
  form1.removal(m);
 end;

procedure SerialBufferStateChange(func: TYSerialPort; value:string);
 begin
  form1.BufferStateChange();
 end;

procedure TForm1.arrival(m:TYModule);
 var
   serial : string;
   f :TYSerialPort;
 begin
   serial:=m.get_serialNumber();
   f := YfindSerialPort(serial+'.serialPort');
   if (f.isOnline()) then
     begin
       f.reset();
       combobox1.items.addObject(f.get_friendlyName(),m);
       if (combobox1.Items.count=1) then combobox1.itemIndex:=0;
     end;
   refreshCurrentPort();
 end;

procedure TForm1.BufferStateChange();
 var line,data,dataline:string;
    p:integer;
  begin
   repeat
     data     := currentPort.readstr(1024);
     dataline := data;
     repeat
       p := pos(dataline,#13);
       if (p>0) then
         begin
            line := copy(dataline,1,p-1);
            dataline := copy(dataline,p,length(dataline)-p);
            if (memo1.lines.count<=0) then memo1.lines.add(line)
                 else memo1.lines[memo1.lines.count-1]:= memo1.lines[memo1.lines.count-1] + line;
            memo1.lines.add('');
         end
         else
           begin
              if (memo1.lines.count<=0) then memo1.lines.add(dataline)
               else memo1.lines[memo1.lines.count-1]:= memo1.lines[memo1.lines.count-1] + dataline;
              dataline := '';
           end;
     until  dataline = '';
   until data='';
 end;

procedure TForm1.removal(m:TYModule);
 var
   index:integer;
 begin
   index := combobox1.items.IndexOfObject(m);
   if (index>=0) then combobox1.items.delete(index);
   if combobox1.items.count<0 then    combobox1.itemIndex:=-1;
   combobox1.refresh();
   refreshCurrentPort();
 end;


procedure TForm1.refreshCurrentPort();
 var
   m:Tymodule;
   serial:string;
   lastPort : TYserialPort;
 begin
    lastport := currentPort;
    if (combobox1.Items.count<=0) or (combobox1.itemIndex<0) then
     begin
       currentPort      := nil;
       setupBtn.enabled := false;
     end
     else
     begin
       m := TYmodule(combobox1.items.objects[combobox1.itemIndex]);
       serial := m.get_SerialNumber();
       currentPort := YfindSerialPort(serial+'.serialPort');
       currentPort.registerValueCallback(SerialBufferStateChange);
       setupBtn.enabled := true;
     end;
   if (lastport<>nil) and (lastport<>currentPort) then
     begin
     memo1.clear;
     edit1.clear;
     tosend:='';
     lastport.registerValueCallBack(nil);
     end;

 end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  refreshCurrentPort();
end;

procedure TForm1.Timer1Timer(Sender: TObject);
 var errmsg: string;
begin
  yUpdateDeviceList(errmsg);
end;

procedure TForm1.Timer2Timer(Sender: TObject);
var
   errmsg:string;
begin
  yHandleEvents(errmsg);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 tosend:='';
 currentPort:=nil;
 yRegisterDeviceArrivalCallback(deviceArrival);
 yRegisterDeviceRemovalCallback(deviceRemoval);
end;





procedure TForm1.Memo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    if (key>=37) and (key<=40) then key:=0;    // disable arrows
    if (key=13) then
       begin
        currentPort.writeStr(tosend+#10);
        memo1.lines.add('');
        tosend:='';
        key:=0;
    end;
end;

procedure TForm1.Memo1KeyPress(Sender: TObject; var Key: Char);
begin
  if (currentPort<>nil) then
   begin
     memo1.lines[memo1.lines.count-1]:= memo1.lines[memo1.lines.count-1] + key;
     tosend:=tosend+key;
     
   end;

end;

procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
    if key=#13 then
       begin
        currentPort.writeStr(edit1.text+#13#10);
        edit1.text :='';
     end;
end;

procedure TForm1.setupBtnClick(Sender: TObject);
begin
  form2.showit(currentPort);
end;

end.
