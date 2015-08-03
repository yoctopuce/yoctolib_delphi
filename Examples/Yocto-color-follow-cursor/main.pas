unit main;


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  yocto_api,yocto_colorled, StdCtrls, ExtCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    ComboBox1: TComboBox;
    Label1: TLabel;
    Timer1: TTimer;
    Label2: TLabel;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    Pxl         : tbitmap;
    scanCounter : integer;
    function  getPixelColor(myXcoord,myYcoord:integer):integer;
    function  capturePixelColorAtMousePosition():integer;
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
     if copy(name,1,8)='YRGBLED1' then
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

  if combobox1.items.count =0 then  StatusBar1.panels[0].text:='No device connected'
  else if combobox1.items.count =1 then  StatusBar1.panels[0].text:='One device connected'
  else StatusBar1.panels[0].text:=intToStr(combobox1.items.count)+' devices connected';

 end;

function TForm1.getPixelColor(myXcoord,myYcoord:integer):integer;
var
   dc:integer;
begin
  // retreive pixel color at specific screen position
  dc:=GetDC(0);
  BitBlt(Pxl.Canvas.Handle,0,0,1,1,dc,myXcoord ,myYcoord,SRCCOPY);
  getPixelColor:=Pxl.Canvas.Pixels[0,0];
  releaseDC(0,dc);
end;

function TForm1.capturePixelColorAtMousePosition():integer;
 var pt:Tpoint;
  color:integer;
begin
  GetCursorPos(pt);
  color:= getPixelColor(pt.x,pt.y);
  color :=    ((color AND $FF) SHL 16)
           OR  (color AND $FF00)
           OR  ((color AND $FF0000) SHR 16);
  capturePixelColorAtMousePosition :=  color;
end;


procedure TForm1.Timer1Timer(Sender: TObject);
var
  color:integer;
  led : TyColorLed;
  module:Tymodule;
  errmsg:string;
begin
  // captute the color and set the leds  of the selected modules
  timer1.enabled:=false;
  if combobox1.enabled then
    if (combobox1.itemindex>=0) then
    begin
      color  := capturePixelColorAtMousePosition();
      module := tyModule(combobox1.items.objects[combobox1.itemindex]);
      led    := yFindColorLed(module.get_serialNumber()+'.colorLed1');
      if (led.isOnline()) then
         begin
          led.set_rgbColor(color);
          led    := yFindColorLed(module.get_serialNumber()+'.colorLed2');
          led.set_rgbColor(color);
         end
    end;

   // form time to time force a device scan, devicelistchanged
   // will be called if something interrestings happened
   inc(scanCounter);
   if (scanCounter>10)  then
    begin
      yUpdateDeviceList(errmsg); // scan for changes
      scanCounter :=0;
    end;

   timer1.enabled:=true;
end;


procedure TForm1.FormCreate(Sender: TObject);
 
 begin
   scanCounter:=0;

   Pxl := tbitmap.create();  // used for color capture
   Pxl.Width:=1;
   Pxl.Height:=1;
   ModulesInventory();
   // we waana know whant device list changes
   yRegisterDeviceArrivalCallBack( devicelistchanged);
   yRegisterDeviceRemovalCallBack( devicelistchanged);
   timer1.enabled:=true;
 end;

 var
   errmsg: string;
// some hardware init
initialization


   if yRegisterHub('usb', errmsg)<>YAPI_SUCCESS then
    begin
      Application.MessageBox(pchar('Yocto-api init failed: '+errmsg), 'Error', MB_OK);
      application.terminate();
    end;
   yDisableExceptions();
end.
