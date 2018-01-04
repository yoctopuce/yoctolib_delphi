unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, ComCtrls, ExtCtrls, StdCtrls, yocto_api, yocto_relay;

type
  TForm1 = class(TForm)
    ImageList1: TImageList;
    ComboBox1: TComboBox;
    Label1: TLabel;
    deviceScanTimer: TTimer;
    deviceImage: TImage;
    StatusBar1: TStatusBar;
    BtA: TButton;
    BtB: TButton;
    BtBeacon: TButton;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure deviceScanTimerTimer(Sender: TObject);
    procedure BtAClick(Sender: TObject);
    procedure BtBClick(Sender: TObject);
    procedure BtBeaconClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);

  private
    { Private declarations }
     timerindex :integer;
     Procedure setImage(index:integer);
     procedure toggleBeacon();
     procedure switch(state:integer);

  public
    { Public declarations }
     Procedure modulesInventory();
  end;

var
  Form1: TForm1;



implementation

{$R *.DFM}


procedure devicelistchanged(m:TyModule);

 begin
   // something has changed in the devices list
   // lets refresh it, quick and dirty way.
   form1.modulesInventory();
   
 end;


Procedure TForm1.modulesInventory();

  var
   module         : TYModule;
   name,lname     : string;
   currentModule  : Tymodule;
   index,i        : integer;
 begin

   // memorize the current selection
   currentModule := nil;
   if (combobox1.itemindex>=0) then
       currentModule := TYModule(combobox1.items.objects[combobox1.itemindex]);

   // update the list, brute force
   combobox1.items.clear;
   module := yFirstModule();
   while module<>nil  do
   begin
     name  :=  module.get_serialNumber();
     if copy(name,1,8)='RELAYHI1' then
      begin
       lname :=  module.get_logicalName();
       if (lname<>'') then  name:=name+' ('+lname+')';
       combobox1.items.AddObject(name,module);
      end;
      module := module.nextModule();
   end;

   // restore previous selection
   if (combobox1.items.count=0) then
    begin
      combobox1.enabled:=false;
    end
    else
    begin
     combobox1.enabled:=true;
     index :=0;
     for i:=0 to combobox1.items.count-1 do
       if  (combobox1.items.objects[i]=currentModule) then index:=i;
     combobox1.itemindex:=index;
    end;

   bta.enabled      := combobox1.enabled;
   btb.enabled      := combobox1.enabled;
   btbeacon.enabled := combobox1.enabled;

   if combobox1.items.count =0 then  StatusBar1.panels[0].text:='Connect a Yocto-PowerRelay device'
   else if combobox1.items.count =1 then  StatusBar1.panels[0].text:='One device connected'
   else StatusBar1.panels[0].text:=intToStr(combobox1.items.count)+' devices connected';

end;

Procedure TForm1.setImage(index:integer);
 var
  image:tbitmap;
 begin
 image := tbitmap.create();
 imagelist1.GetBitmap(index,image);
 image.canvas.brush.style:=bsclear;
 image.canvas.rectangle(rect(0,0,image.width,image.height));
 deviceimage.picture.assign(image);
 image.free();

 end;

procedure TForm1.FormCreate(Sender: TObject);

begin
   ModulesInventory();
   // we waana know when device list changes
   yRegisterDeviceArrivalCallBack( devicelistchanged);
   yRegisterDeviceRemovalCallBack( devicelistchanged);
   setImage(4);
   deviceScanTimer.enabled:=true;
end;

procedure TForm1.deviceScanTimerTimer(Sender: TObject);
 var errmsg: string;
begin
    yUpdateDeviceList(errmsg); // scan for devices list changes
end;


procedure TForm1.ToggleBeacon();
 var
   module :TyModule;

 begin
   if  (not(combobox1.enabled) or (combobox1.itemindex<0)) then  exit;
   module:=  TyModule(combobox1.items.objects[combobox1.itemindex]);
   if module.isOnline() then
     begin
       if (module.get_beacon()=Y_BEACON_ON) then  module.set_beacon(Y_BEACON_OFF)
                                            else   module.set_beacon(Y_BEACON_ON);
     end;
 end;

procedure TForm1.Switch(state:integer);
 var
   module :TyModule;
   relay  :TyRelay;
 begin
  if  (not(combobox1.enabled) or (combobox1.itemindex<0)) then  exit;
  module:=  TyModule(combobox1.items.objects[combobox1.itemindex]);
  relay:= yFindRelay(module.get_SerialNumber()+'.relay1');
  if relay.isOnline() then relay.set_state(state);
 end;

procedure TForm1.BtAClick(Sender: TObject);
begin
  Switch(Y_STATE_A);
end;

procedure TForm1.BtBClick(Sender: TObject);
begin
  Switch(Y_STATE_B);
end;


procedure TForm1.BtBeaconClick(Sender: TObject);
begin
ToggleBeacon();
end;


var
   errmsg: string;


procedure TForm1.Timer1Timer(Sender: TObject);
 var
  index        : integer;
  module       : tymodule;
  relay        : tyrelay;
  state,beacon : integer;
  ok           : boolean;
begin

  module     := nil;
  index      := 4;
  ok         := true;
  beacon     := 0;
  state      := 0;
  timerindex := (timerindex+1) mod 3;


 if  (not(combobox1.enabled) or (combobox1.itemindex<0)) then  ok:=false;
 if ok then  module:=  TyModule(combobox1.items.objects[combobox1.itemindex]);
 if ok then
  begin
    relay:= yFindRelay(module.get_SerialNumber()+'.relay1');
    if relay.isOnline() then
      begin
        state  := relay.get_state();
        beacon := module.get_beacon();
      end else ok:=false;
  end;

 if (beacon =Y_BEACON_ON) and   (timerindex>0) then  beacon:=Y_BEACON_OFF;

 if ok then
  begin
    if  state= Y_STATE_A then
      begin
       if beacon =Y_BEACON_ON then index:=1 else    index:=0;
      end
    else
      begin
       if beacon =Y_BEACON_ON then index:=3 else    index:=2;
      end;
   end;

  setImage(index);

end;

initialization
  //  API init, 'usb' parameter is for local devices, but you can also
  //  use a virtual hub addr (ex: http://127.0.0.1:4444)

  if yRegisterHub('usb', errmsg)<>YAPI_SUCCESS then
    begin
      Application.MessageBox(pchar('Yocto-api init failed: '+errmsg), 'Error', MB_OK);
      application.terminate();
    end;
   yDisableExceptions();


end.
