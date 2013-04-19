unit preview;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,yocto_api,yocto_display;

type
  TpreviewForm = class(TForm)
    Panel1: TPanel;
    DevList: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Timer1: TTimer;
    Label3: TLabel;
    messageField: TEdit;
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DevListChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
     bm      : TBYTEARRAY;
    bmWidth : integer;
    currentDisplay : Tydisplay;
    currentDisplayWidth: integer;
    currentDisplayHeight: integer;

    procedure devicearrival(m:Tymodule);
    procedure deviceremoval(m:Tymodule);


  public
    procedure setPreviewData(data:array of byte; width:integer);
    function  getMessage:string;
    { Public declarations }
  end;

var
  previewForm: TpreviewForm;

implementation

{$R *.DFM}

procedure    arrival(m:Tymodule);
 begin
   previewForm.devicearrival(m);
 end;

procedure    removal(m:Tymodule);
 begin
   previewForm.deviceremoval(m);
 end;


procedure TpreviewForm.devicearrival(m:Tymodule);
  var
    n,serial :string;
    index:integer;
    d:TYdisplay;
  begin
    n:=m.get_FriendlyName();
    serial:=m.get_SerialNumber();
    d := YFindDisplay(serial+'.display');
    if d.isOnline() then
     begin
      index:=DevList.items.add(n);
      DevList.items.objects[index]:=m;
      if (DevList.ItemIndex<0) then  DevList.ItemIndex:=0;
      DevListChange(nil);
     end;
  end;

procedure TpreviewForm.deviceremoval(m:Tymodule);
  var
   i :integer;
  begin
    for i:=DevList.items.count-1 downto 0 do
     if  DevList.items.objects[i]=m then
       DevList.items.delete(i);
  end;

procedure TpreviewForm.FormShow(Sender: TObject);
 var
  errmsg :string;
begin
 if YregisterHub('usb',errmsg) <> YAPI_SUCCESS  then
  begin
    panel1.visible:=false;
    label1.caption:='Cannot init Yoctopuce API '+errmsg;
    exit;
  end;
  yRegisterDeviceArrivalCallback(arrival);
  yRegisterDeviceRemovalCallback(removal);
  panel1.visible:=true;
  timer1.enableD:=true;

end;

procedure TpreviewForm.Timer1Timer(Sender: TObject);
var
  errmsg : string;
  l      : Tydisplaylayer;
   h: integer;
begin
   yUpdateDeviceList(errmsg);
   if currentDisplay<>nil then
     if currentDisplay.isonline() then
       if bmwidth>0 then
        begin
          h := length(bm) div ((bmwidth+7)div 8) ;
          l := currentDisplay.get_DisplayLayer(1);
          l.clear;
          l.drawbitmap( (currentDisplayWidth - bmwidth) div 2 , (currentDisplayHeight - h) div 2,bmwidth, bm , -1 );
          currentDisplay.swapLayerContent(0,1);
       end;
end;

procedure TpreviewForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DevList.items.clear;
  timer1.enableD:=false;
  yfreeAPi();
end;

function  TpreviewForm.getMessage:string;
 begin
    getMessage := messageField.text;
 end;

procedure  TpreviewForm.setPreviewData(data:array of byte; width:integer);
 var
  i : integer;
 begin
    SetLength(bm, length(data));
    for i:=0  to length(data)-1 do bm[i]:=data[i];
    bmwidth:=width;
 end;

procedure TpreviewForm.DevListChange(Sender: TObject);
var
  m : TYmodule;
  serial : string;
 
begin
  m             := TYModule(Devlist.items.objects[Devlist.itemindex]);
  serial        := m.get_serialNumber();
  currentDisplay:= YfindDisplay(serial+'.display');
  if currentDisplay.isonline() then
    begin
      currentDisplay.resetAll();
      currentDisplayWidth:= currentDisplay.get_DisplayWidth();
      currentDisplayHeight:=currentDisplay.get_DisplayHeight();

    end;

end;

procedure TpreviewForm.FormCreate(Sender: TObject);
begin
  currentDisplay:=nil;
end;

end.
