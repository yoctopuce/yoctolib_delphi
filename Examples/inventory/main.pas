unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, yocto_api, ImgList;

type
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    ImageList1: TImageList;
    Timer2: TTimer;
    ListView1: TListView;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    { Private declarations }
    beaconsRefreshIndex :integer;
    Procedure beaconsRefresh();
    procedure RefreshStatusBar();
  public
    { Public declarations }
    Procedure moduleArrival(m:tymodule);
    Procedure moduleLeft(m:tymodule);


  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure devicelistArrival(m:tymodule);
begin
  // A module just arrived, lets add it  (incremental way)
  form1.moduleArrival(m);

end;

procedure devicelistDeviceLeft(m:tymodule);
begin
  // A module just arrived, lets remove it (incremental way)
  form1.moduleLeft(m);

end;

Procedure Tform1.RefreshStatusBar();
 begin
  if listview1.items.count =0 then  StatusBar1.panels[0].text:='No device connected'
  else if listview1.items.count =1 then  StatusBar1.panels[0].text:='One device connected'
  else StatusBar1.panels[0].text:=intToStr(listview1.items.count)+' devices connected';
 end;

Procedure Tform1.moduleArrival(m:tymodule);
 var
   item : TListItem;
   i    : integer;
 begin
 // just in case , lets make sure the module is not already there
   for i:=0 to listview1.items.count-1 do
    if  listview1.items[i].data = m then  exit;

    StatusBar1.panels[1].text := m.get_serialNumber()+' has been connected';

   item:=listview1.items.add();
   item.data       :=m;
   item.Caption    :=m.Get_serialNumber();
   item.subitems.add(m.get_productName());
   item.subitems.add(m.get_LogicalName());
   RefreshStatusBar();
 end;

Procedure Tform1.moduleLeft(m:tymodule);
 var
   i    : integer;

begin
  for i:=listview1.items.count-1  downto 0  do
   if  listview1.items[i].data = m then
     listview1.items.delete(i);

   StatusBar1.panels[1].text := m.get_serialNumber()+' has been disconnected';
   RefreshStatusBar();
end;


Procedure Tform1.beaconsRefresh();
 var
 index    : integer;
 Module   : tymodule;
 begin
  if (listview1.items.count<=0) then exit;
  beaconsRefreshIndex := (beaconsRefreshIndex +1) mod listview1.items.count;
  module:= TyModule(listview1.items[beaconsRefreshIndex].data);
  if module.isOnline() then
    begin
     index :=0;
     if (module.get_beacon()= Y_BEACON_ON) then Index:=1;
     if  listview1.items[beaconsRefreshIndex].ImageIndex<>index then
         listview1.items[beaconsRefreshIndex].ImageIndex:=index;
    end;
 end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
 beaconsRefresh();
end;


procedure TForm1.Timer1Timer(Sender: TObject);
 var errmsg:String;
begin
  // scan for changes, if will  call devicelistArrival
  // and devicelistDeviceLeft if needed
  yUpdateDeviceList(errmsg);
  beaconsRefresh();
end;

procedure TForm1.FormCreate(Sender: TObject);

begin
  // devicelistArrival and  devicelistDeviceLeft will be automatically
  // called right after yUpdateDeviceList call if there is some changes
  // in the device list.  devicelistArrival and   devicelistDeviceLeft
  // must be regular procedures, not methods.
  yRegisterDeviceArrivalCallBack( devicelistArrival);
  yRegisterDeviceRemovalCallBack( devicelistDeviceLeft);
  RefreshStatusBar();
  timer1.enabled:=true;
end;

var
 errmsg : string;

initialization
  if yRegisterHub('usb', errmsg)<>YAPI_SUCCESS then
    begin
      Application.MessageBox(pchar('Yocto-api init failed: '+errmsg), 'Error', MB_OK);
      application.terminate();
    end;
   yDisableExceptions();
end.
