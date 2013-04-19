unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, yocto_api,Yocto_anButton, ImgList;

type
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    level1: TProgressBar;
    calibrate1: TButton;
    Label1: TLabel;
    pressed1: TImage;
    level2: TProgressBar;
    calibrate2: TButton;
    Label2: TLabel;
    pressed2: TImage;
    level3: TProgressBar;
    calibrate3: TButton;
    Label3: TLabel;
    pressed3: TImage;
    level4: TProgressBar;
    calibrate4: TButton;
    Label4: TLabel;
    pressed4: TImage;
    level5: TProgressBar;
    calibrate5: TButton;
    Label5: TLabel;
    pressed5: TImage;
    Label6: TLabel;
    ComboBox1: TComboBox;
    ImageList1: TImageList;
    DeviceScan: TTimer;
    refreshTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure DeviceScanTimer(Sender: TObject);
    procedure refreshTimerTimer(Sender: TObject);
  private
    { Private declarations }
    level_indicators:     array[1..5] of  TProgressBar;
    calibrate_buttons:    array[1..5] of  TButton;
    pressed_indicators:   array[1..5] of  TImage;
    labels:               array[1..5] of  TLabel;
  
    Procedure calibrationPressed(source:Tobject);
    Procedure setIndicator(icon:Timage;index:integer);
    Procedure refresh();
  public
    Procedure modulesInventory();

    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure devicelistchanged(m:tyModule);

 begin
   // something has changed in the devices list
   // lets refresh it, quick and dirty way.
   form1.modulesInventory();
 end;

Procedure TForm1.CalibrationPressed(source:Tobject);
 var
  index,i  : integer;
  module   : TyModule;
  anButton : TyAnButton;

 begin
  index:=-1;
  for i:=1 to 5 do
    if calibrate_buttons[i]=source then index:=i;
  if (index<0) then exit;

  if ((combobox1.itemindex<0) or not(combobox1.enabled)) then exit;
  module :=  TyModule(combobox1.items.objects[combobox1.itemindex]);
  
  if module.isOnline() then
   begin
     anButton := yFindAnButton(module.get_SerialNumber()+'.anButton'+intToStr(index));
     if (anButton.get_analogCalibration() = Y_ANALOGCALIBRATION_ON) then
              anButton.set_analogCalibration(Y_ANALOGCALIBRATION_OFF)
        else  anButton.set_analogCalibration(Y_ANALOGCALIBRATION_ON)
   end;

 end;

Procedure TForm1.setIndicator(icon:Timage;index:integer);
 var
  bmp:tbitmap;
 begin
  if (icon.tag =index) then exit;
  bmp := tbitmap.create();
  imagelist1.GetBitmap(index,bmp);
  icon.picture.assign(bmp);
  bmp.free();
  icon.tag := index;
 end;

Procedure TForm1.refresh();
 var
   module    :TyModule;
   anButton : TyAnButton;
   i : integer;
   serial :string;
 begin
   if ((combobox1.itemindex<0) or not(combobox1.enabled)) then exit;
   module:=  TyModule(combobox1.items.objects[combobox1.itemindex]);
   serial:= module.get_SerialNumber();
   if module.isOnline() then
   for i:=1 to 5 do
     begin
       anButton:=  yFindAnButton(serial+'.anButton'+intToStr(i));
       level_indicators[i].position :=  anButton.get_calibratedValue() div 10;
       if (anButton.get_analogCalibration() = Y_ANALOGCALIBRATION_ON) then
             calibrate_buttons[i].caption:='Stop calibration'
        else calibrate_buttons[i].caption:='Start calibration';
       if (anButton.get_isPressed() =Y_ISPRESSED_TRUE)  then
             setIndicator( pressed_indicators[i],2)
        else setIndicator( pressed_indicators[i],1);
     end;
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
     if copy(name,1,8)='YBUTTON1' then
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

   for i:=1 to  5 do
    begin
     labels[i].enabled              := combobox1.enabled;
     level_indicators[i].position   := 0;
     level_indicators[i].enabled    := combobox1.enabled;
     calibrate_buttons[i].enabled   := combobox1.enabled;
     setIndicator( pressed_indicators[i],0);

    end;

     if combobox1.items.count =0 then  StatusBar1.panels[0].text:='Plug a Yocto-Knob device'
  else if combobox1.items.count =1 then  StatusBar1.panels[0].text:='One device connected'
  else StatusBar1.panels[0].text:=intToStr(combobox1.items.count)+' devices connected';

end;

procedure TForm1.FormCreate(Sender: TObject);
 var
 
  i      : integer;
begin
 level_indicators[1]:=level1;      calibrate_buttons[1]:=calibrate1;
 pressed_indicators[1]:= pressed1; labels[1]:= label1;

 level_indicators[2]:=level2;      calibrate_buttons[2]:=calibrate2;
 pressed_indicators[2]:= pressed2; labels[2]:= label2;

 level_indicators[3]:=level3;      calibrate_buttons[3]:=calibrate3;
 pressed_indicators[3]:= pressed3; labels[3]:= label3;

 level_indicators[4]:=level4;      calibrate_buttons[4]:=calibrate4;
 pressed_indicators[4]:= pressed4; labels[4]:= label4;

 level_indicators[5]:=level5;      calibrate_buttons[5]:=calibrate5;
 pressed_indicators[5]:= pressed5; labels[5]:= label5;

 ModulesInventory();

 // we wanna know when device list changes
 yRegisterDeviceArrivalCallBack( devicelistchanged);
 yRegisterDeviceRemovalCallBack( devicelistchanged);

 for i:=1 to 5 do   calibrate_buttons[i].onclick := CalibrationPressed;

 DeviceScan.enabled   :=true;
 refreshTimer.enabled :=true;
end;


procedure TForm1.DeviceScanTimer(Sender: TObject);
 var errmsg: string;
begin
   yUpdateDeviceList(errmsg); // scan for devices list changes
end;

var
 errmsg :string;
procedure TForm1.refreshTimerTimer(Sender: TObject);
begin
 refresh();
end;

initialization
  if yRegisterHub('usb', errmsg)<>YAPI_SUCCESS then
    begin
      Application.MessageBox(pchar('Yocto-api init failed: '+errmsg), 'Error', MB_OK);
      application.terminate();
    end;
   yDisableExceptions();

end.
