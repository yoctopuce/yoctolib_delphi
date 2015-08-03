unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  yocto_api,yocto_servo, StdCtrls, ComCtrls, ExtCtrls, ImgList;
type
  TForm1 = class(TForm)
    ComboBox1: TComboBox;
    StatusBar1: TStatusBar;
    Label1: TLabel;
    InventoryTimer: TTimer;
    Beacon: TCheckBox;
    refreshTimer: TTimer;
    TrackBar1: TTrackBar;
    Label2: TLabel;
    Label3: TLabel;
    TrackBar2: TTrackBar;
    Label4: TLabel;
    TrackBar3: TTrackBar;
    Label5: TLabel;
    TrackBar4: TTrackBar;
    Label6: TLabel;
    TrackBar5: TTrackBar;
    allatonce: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure InventoryTimerTimer(Sender: TObject);
    procedure refreshTimerTimer(Sender: TObject);
    procedure BeaconClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar1Enter(Sender: TObject);
    procedure TrackBar1Exit(Sender: TObject);

  private
    { Private declarations }
    trackBars : array[0..4] of Ttrackbar;
    MaxCurrent     : integer;
    Procedure refresh();

  public
    { Public declarations }
      Procedure modulesInventory();
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}


procedure devicelistchanged(m:Tymodule);

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
     if copy(name,1,8)='SERVORC1' then
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
      statusbar1.panels[0].text:='Connect a Yocto-Demo device';
    end
    else
    begin
     combobox1.enabled:=true;
     index :=0;
     for i:=0 to combobox1.items.count-1 do
       if  (combobox1.items.objects[i]=currentModule) then index:=i;

     if (combobox1.items.count =1) then
       statusbar1.panels[0].text:='One Yocto-Servo device connected'
      else
        statusbar1.panels[0].text:=intToStr(combobox1.items.count)+' Yocto-Servo devices connected';
     combobox1.itemindex:=index;
    end;

   beacon.enabled := combobox1.enabled;
   for i:=0 to 4 do
     trackBars[i].enabled:=combobox1.enabled;
   label2.enabled:= combobox1.enabled;
   label3.enabled:= combobox1.enabled;
   label4.enabled:= combobox1.enabled;
   label5.enabled:= combobox1.enabled;
   label6.enabled:= combobox1.enabled;

   if combobox1.items.count =0 then  StatusBar1.panels[0].text:='Connect a Yocto-Servo device'
   else if combobox1.items.count =1 then  StatusBar1.panels[0].text:='One device connected'
   else StatusBar1.panels[0].text:=intToStr(combobox1.items.count)+' devices connected';

 end;

procedure TForm1.TrackBar1Change(Sender: TObject);
 var
  module   : TYModule;
  servo    : TYServo;
  index    : integer;
  value,i  : integer;
begin
  if  (not(combobox1.enabled) or (combobox1.itemindex<0)) then  exit;
  module := TYModule(combobox1.items.objects[combobox1.itemindex]);
  value  := TTrackBar(sender).position;

  if  (allatonce.checked) then
   for i:=1 to 5 do
   begin
     servo  := yFindServo(module.get_serialNumber()+'.servo'+inttostr(i));
     if (servo.isOnline())  then servo.set_position(value);
   end
   else
   begin
     index  := TTrackBar(sender).tag+1;
     servo  := yFindServo(module.get_serialNumber()+'.servo'+inttostr(index));
     if (servo.isOnline())  then servo.set_position(value);
   end;
end;


procedure TForm1.TrackBar1Enter(Sender: TObject);
begin
  TTrackBar(sender).onchange := TrackBar1Change;
end;

procedure TForm1.TrackBar1Exit(Sender: TObject);
begin
  TTrackBar(sender).onchange := nil;
end;

procedure TForm1.FormCreate(Sender: TObject);
var

  i:integer;
begin
  trackBars[0]:=trackBar1;
  trackBars[1]:=trackBar2;
  trackBars[2]:=trackBar3;
  trackBars[3]:=trackBar4;
  trackBars[4]:=trackBar5;
 MaxCurrent:=0;
  ModulesInventory();
   // we wanna know when device list changes
  yRegisterDeviceArrivalCallBack( devicelistchanged);
  yRegisterDeviceRemovalCallBack( devicelistchanged);

  for i:=0 to 4 do
    begin
      trackBars[i].tag:=i;
      trackBars[i].onEnter := TrackBar1Enter;
      trackBars[i].onExit  := TrackBar1Exit
    end;
  InventoryTimer.enabled:=true;
  refreshTimer.enabled:=true;
end;

procedure TForm1.InventoryTimerTimer(Sender: TObject);
 var
  errmsg:string;
begin
  yUpdateDeviceList(errmsg); // scan for changes
end;

Procedure TForm1.refresh();
 var
   module      : TyModule;
   servo       : TyServo;
   i            : integer;
   current     : integer;
 begin

   if  (not(combobox1.enabled) or (combobox1.itemindex<0)) then  exit;
   module := TYModule(combobox1.items.objects[combobox1.itemindex]);

   Beacon.onclick:=nil;
   if  module.get_beacon() = Y_BEACON_ON then
      begin
        beacon.checked:=  true;
       end else beacon.checked:=  false;
   Beacon.onclick:=BeaconClick;

   for i:=1 to 5 do
    if not(assigned(trackBars[i-1].onChange)) then
    begin
     servo := yFindServo(module.get_serialNumber()+'.servo'+intToStr(i));
     if (servo.isOnline()) then
         trackBars[i-1].position:=servo.get_position();
    end;

    if (module.isOnline()) then
    begin
     current := module.get_usbCurrent();
     if (current > MaxCurrent) then MaxCurrent:=current;
     statusbar1.panels[1].text:= intToStr(current)+'mA (Max: '+ intToStr(maxCurrent)+')';
    end;
 end;

procedure TForm1.refreshTimerTimer(Sender: TObject);
begin
  refresh();
end;

procedure TForm1.BeaconClick(Sender: TObject);
 var module:TyModule;
begin
  if (not(combobox1.enabled) or (combobox1.itemindex<0)) then exit;
  module:=  TyModule(combobox1.items.objects[combobox1.itemindex]);
  if not(module.isOnline()) then exit;
   if  module.get_beacon() = Y_BEACON_OFF then module.set_beacon(Y_BEACON_ON)
                                          else module.set_beacon(Y_BEACON_OFF);

end;



var
   errmsg: string;




initialization
  if yRegisterHub('usb', errmsg)<>YAPI_SUCCESS then
    begin
      Application.MessageBox(pchar('Yocto-api init failed: '+errmsg), 'Error', MB_OK);
      application.terminate();
    end;
   yDisableExceptions();

end.
