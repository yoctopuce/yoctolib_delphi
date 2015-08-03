unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  yocto_api,yocto_led, StdCtrls, ComCtrls, ExtCtrls, ImgList;
type
  TForm1 = class(TForm)
    ComboBox1: TComboBox;
    StatusBar1: TStatusBar;
    Label1: TLabel;
    InventoryTimer: TTimer;
    Beacon: TCheckBox;
    led: TCheckBox;
    deviceimage: TImage;
    refreshTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure InventoryTimerTimer(Sender: TObject);
    procedure refreshTimerTimer(Sender: TObject);
    procedure BeaconClick(Sender: TObject);
    procedure ledClick(Sender: TObject);
  private
    { Private declarations }
    icons : array[0..4] of Tbitmap;
    blinkindex :integer;
    Procedure refresh();

  public
    { Public declarations }
      Procedure modulesInventory();
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}
{$R images}

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
     if copy(name,1,8)='YCTOPOC1' then
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
       statusbar1.panels[0].text:='One Yocto-Demo device connected'
      else
        statusbar1.panels[0].text:=intToStr(combobox1.items.count)+' Yocto-Demo devices connected';
     combobox1.itemindex:=index;
    end;
   led.enabled    := combobox1.enabled;
   beacon.enabled := combobox1.enabled;
 end;

procedure TForm1.FormCreate(Sender: TObject);
var

  i:integer;
const
  iconname : array[0..4] of string = ('poc','pocg','pocb','pocbg','nopoc');


begin
  for i:=0 to 4 do
   begin
     icons[i] := Tbitmap.create();
     icons[i].LoadFromResourceName(hInstance,iconname[i]);
     icons[i].canvas.brush.style:=bsclear;
     icons[i].canvas.rectangle(rect(0,0,icons[i].width,icons[i].height));
   end;
  deviceimage.picture.assign(icons[4]);
  ModulesInventory();
   // we wanna know when device list changes
  yRegisterDeviceArrivalCallBack( devicelistchanged);
  yRegisterDeviceRemovalCallBack( devicelistchanged);
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
   module    : TyModule;
   testled   : TyLed;
   ok        : boolean;
   iconindex : integer;
 begin
   ok:=false;
   module:=nil;
   iconindex := 0;
   if  (not(combobox1.enabled) or (combobox1.itemindex<0)) then
    begin
     iconindex := 4;
     ok:=false;
    end;

   if ok then  module:=  TyModule(combobox1.items.objects[combobox1.itemindex]);
   if ok then
     begin
       testled:= yFindLed(module.get_SerialNumber()+'.led');
       if testled.isOnline() then
        begin
          led.onclick:=nil;
          if testled.get_power() =  Y_POWER_ON then
            begin
              iconindex   := iconindex or 1;
              led.checked :=  true;
            end else led.checked:=  false;
          led.onclick:=ledClick;

          Beacon.onclick:=nil;
         if  module.get_beacon() = Y_BEACON_ON then
           begin
             if (blinkindex=0) then iconindex := iconindex or 2;
             beacon.checked:=  true;
            end else beacon.checked:=  false;
          Beacon.onclick:=BeaconClick;

      end
   end;
   deviceimage.picture.assign(icons[iconindex]);
 end;

procedure TForm1.refreshTimerTimer(Sender: TObject);
begin
  blinkindex:=(blinkindex+1) mod 3 ;
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

procedure TForm1.ledClick(Sender: TObject);
var
 testled:TyLed;
 module:TyModule;
begin
  if (not(combobox1.enabled) or (combobox1.itemindex<0)) then exit;
  module:=  TyModule(combobox1.items.objects[combobox1.itemindex]);
  if not(module.isOnline()) then exit;
  testled:=yFindLed( module.get_SerialNumber+'.led');
  if  testled.get_power() = Y_POWER_OFF then testled.set_power(Y_POWER_ON)
                                       else testled.set_power(Y_POWER_OFF);
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
