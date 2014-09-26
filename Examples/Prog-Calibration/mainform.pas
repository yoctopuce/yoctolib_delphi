
//
//  Generic UI to calibrate Yocto sensors using integrated
//  1-5 points linear interpolation. Most of the code is UI
//  handling, but the key function is CalibrationChange()
//

unit mainform;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, math, yocto_api;

type
  TForm1 = class(TForm)
    devicesList: TComboBox;
    functionsList: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    ValueDisplay: TLabel;
    InventoryTimer: TTimer;
    ValueDisplayUnits: TLabel;
    R0: TEdit;
    C0: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    R1: TEdit;
    C1: TEdit;
    R2: TEdit;
    C2: TEdit;
    R3: TEdit;
    C3: TEdit;
    R4: TEdit;
    C4: TEdit;
    saveBtn: TButton;
    StatusBar1: TStatusBar;
    cancelBtn: TButton;
    Label5: TLabel;
    Label6: TLabel;
    unsupported_warning: TLabel;
    nosensorfunction: TLabel;
    RawValueDisplay: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure InventoryTimerTimer(Sender: TObject);
    procedure devicesListChange(Sender: TObject);
    procedure functionsListChange(Sender: TObject);
    procedure CalibrationChange(Sender: TObject);
    procedure saveBtnClick(Sender: TObject);
    procedure cancelBtnClick(Sender: TObject);
  private
    { Private declarations }
    caledit   : array [0..4] of Tedit;
    rawedit   : array [0..4] of Tedit;
    Procedure  arrival(module:TYModule);
    Procedure  removal(module:TYModule);
    procedure  choosenDeviceChanged();
    procedure  refreshFctUI(newone:boolean);
    procedure  DisplayValue(fct: TYSensor);
    procedure  DisplayCalPoints(fct: TYSensor);
    Procedure  EnableCalibrationUI(state:boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

// we use simple function callbacks because the API  does
// not support "from object" callback
procedure arrivalCallback(module:TYModule);
 begin
  form1.arrival(module);
 end;

Procedure  removalCallback(module:TYModule);
 begin
   form1.removal(module);
 end;

Procedure    TForm1.arrival(module:TYModule);
 begin
   // add the device in the 1srt combo list
   devicesList.items.addObject(module.get_friendlyname(),module);
   if (devicesList.items.count=1) then
    begin
      devicesList.itemindex:=0;
      choosenDeviceChanged();
    end;
 end;

Procedure    TForm1.removal(module:TYModule);
 var
   index,I     : integer;
   mustrefresh : boolean;
 begin
  // search fo the device in the combo box objects list
  index:=-1;
  for i:=0 to devicesList.items.count-1 do
   if devicesList.items.objects[i]=module then index :=i;

  // if we removed the current module, we must fully refresh the ui
  mustrefresh := index = devicesList.itemindex;

  if (index>=0) then
   begin
    // remove it from the combo box
    devicesList.items.delete(index);
    if  devicesList.itemindex>= devicesList.items.count  then  devicesList.itemindex := 0;
     // if  we  deleted the active device, we need a refresh
     if  mustrefresh then
       begin
        functionsList.enabled:=false;
        choosenDeviceChanged();
       end;
   end;
 end;

procedure  TForm1.choosenDeviceChanged();

 var
  currentDevice  : TYModule;
  fctount,i      : integer;
  fct            : TYSensor;
  fctName        : string;
  fctFullName    : string;

 begin
  devicesList.enabled:=devicesList.items.count>0;
  // clear the functions drop down
  while (functionsList.items.count>0) do functionsList.items.delete(0);

  functionsList.enabled := devicesList.enabled;

  if not(devicesList.enabled) then
  begin
    unsupported_warning.visible:=false;
    nosensorfunction.visible:=false;
     exit;  // no device at all connected,
  end;
  if (devicesList.itemindex<0)  then devicesList.itemindex:=0;
  currentDevice := TYModule(devicesList.items.objects[devicesList.itemindex]);

  // populate the second drop down
  if (currentDevice.isOnline()) then
   begin
     // device capabilities inventory
     fctount := currentDevice.FunctionCount();
     for i:=0 to  fctount-1 do
      begin
       fctName := currentDevice.functionId(i);
       fctFullName := currentDevice.get_serialNumber() + '.' + fctName;
       fct := YFindSensor(fctFullName);
       // add the function in the second drop down
       if (fct.isOnline()) then functionsList.items.addObject(fctName,fct);
     end;
   end;

  functionsList.enabled := functionsList.items.count>0;
  if  (functionsList.enabled) then
   begin
     functionsList.itemindex:=0;
   end;
  refreshFctUI(true);
 end;

procedure TForm1.refreshFctUI(newone:boolean);
var
 fct:TYSensor;
 fctname:string;
 i:integer;
 begin
  nosensorfunction.visible:=false;
  statusBar1.panels[0].text:=inttostr(devicesList.items.count)+' device(s) found';

  if  not(functionsList.enabled)   then
   begin
    // disable the UI
    ValueDisplay.caption:='N/A';
    ValueDisplayUnits.caption:='-';
    RawValueDisplay.caption:='-';
    EnableCalibrationUI(false);
    if   (devicesList.enabled) then
       nosensorfunction.visible:=true
     else
       statusBar1.panels[0].text:='Plug a Yocto-device';
    exit;
   end;

  fct     := TYSensor(functionsList.items.objects[functionsList.itemindex]);
  fctname := functionsList.items[functionsList.itemindex];

  if (newone) then
   begin
    // enable the UI
    EnableCalibrationUI(true);
    for i:=0 to 4 do
      begin
       caledit[i].text:='';
       caledit[i].color:=clwindow;
       rawedit[i].text:='';
       rawedit[i].color:=clwindow;
      end;
    DisplayCalPoints(fct)
   end;
  if (fct.isOnline()) then DisplayValue(fct);
 end;

 procedure Tform1.displayValue(fct: TYSensor);
   var
    value,rawvalue,resolution:double;
    valunit:string;
    l:double;
   begin
    value := fct.get_currentValue();
    rawvalue := fct.get_currentRawValue();
    resolution := fct.get_resolution();
    valunit :=fct.get_unit();
    // displays the sensor value on the ui
    ValueDisplayUnits.caption:=valunit;
    if resolution<>Y_RESOLUTION_INVALID  then
     begin
      // if resolution is available on the device the use it to round the value
      l := round(log10(resolution));
      resolution := power(10,trunc(l));
      RawValueDisplay.caption:='(raw value: '+floatToStr(resolution*round(rawvalue/resolution))+')';
      ValueDisplay.caption:=floatToStr(resolution*round(value/resolution));
     end
     else
     begin
      ValueDisplay.caption:=floatToStr(value);
      RawValueDisplay.caption:='';
     end;
   end;


 // enable /disbale the calibration data edition
 Procedure  TForm1.EnableCalibrationUI(state:boolean);
  var i:integer;
  begin
    for i:=0 to 4 do
      begin
       caledit[i].enabled:=state;
       rawedit[i].enabled:=state;
       if not(state) then
       begin
         caledit[i].text:='';
         rawedit[i].text:='';
         caledit[i].color:=clwindow;
         rawedit[i].color:=clwindow;
       end;
    end;
    label3.enabled:=state;
    label4.enabled:=state;
    saveBtn.enabled:=state;
    cancelBtn.enabled:=state;
 end;

 procedure Tform1.DisplayCalPoints(fct:TYSensor);
 var  i:integer;
      retcode: LongInt;
      ValuesRaw,ValuesCal : floatArr;
 begin
  // little trick: if resolution is not available on the device, the
  // calibration in not available either
  retcode := fct.loadCalibrationPoints(ValuesRaw,ValuesCal);
  if retcode = YAPI_NOT_SUPPORTED then
   begin
     EnableCalibrationUI(false);
     unsupported_warning.visible:=true;
     exit;
  end;

  // display the calibration points
  unsupported_warning.visible:=false;
  for i:=0 to high(ValuesRaw) do
   begin
     rawedit[i].text:=_yapiFloatToStr(ValuesRaw[i]);
     caledit[i].text:=_yapiFloatToStr(Valuescal[i]);
     rawedit[i].color:=$a0FFa0;
     caledit[i].color:=$a0FFa0;
   end;
 end;

procedure TForm1.FormCreate(Sender: TObject);
 var errsmg:string;
begin
   // stores calibration editors  in arrays for easier handling
   caledit[0]:=c0;
   caledit[1]:=c1;
   caledit[2]:=c2;
   caledit[3]:=c3;
   caledit[4]:=c4;

   rawedit[0]:=r0;
   rawedit[1]:=r1;
   rawedit[2]:=r2;
   rawedit[3]:=r3;
   rawedit[4]:=r4;

  //register arrival/removal callbacks
  yRegisterDeviceArrivalCallback(arrivalCallback);
  yRegisterDeviceRemovalCallback(removalCallback);

  // refresh UI
  choosenDeviceChanged();

  // first inventory, arrivalCallback and removalCallback
  // will be called if something changed
  yUpdateDeviceList(errsmg);

  // starts real-time inventory
  InventoryTimer.enabled:=true;

end;

procedure TForm1.InventoryTimerTimer(Sender: TObject);
 var errsmg:string;
begin
  // force an inventory, arrivalCallback and removalCallback
  // will be called if something changed
  yUpdateDeviceList(errsmg);

  // refresh the UI values
  refreshFctUI(false);
end;

procedure TForm1.devicesListChange(Sender: TObject);
begin
  choosenDeviceChanged();
end;

procedure TForm1.functionsListChange(Sender: TObject);
begin
  refreshFctUI(true);
end;

procedure TForm1.CalibrationChange(Sender: TObject);
var
 parseRaw  : TLongIntArray;
 parseCal  : TLongIntArray;
 ValuesRaw : floatArr;
 ValuesCal : floatArr;
 fct       : TYSensor;
 i,j       : integer;
 fctname   : string;

begin
  //  This is the key function: it sets the calibration
  //  data in the device. Note: the parameters are written
  //  in the device RAM, if you want the calibration
  //  to be persistent, you have to call saveToflash();

  //
  // retreive the calibration values from the
  // UI, convert it to float, and make sure they
  // are sorted ascending
  setlength(ValuesRaw,0);
  setlength(ValuesCal,0);
  i:=0;
  try
  while  (i<5) and (caledit[i].text<>'') and (rawedit[i].text<>'') do
   begin
     parseRaw := _decodeFloats(rawedit[i].text);
     parseCal := _decodeFloats(caledit[i].text);
     if (length(parseRaw) <> 1) or (length(parseCal) <> 1) then break;
     if (i>0) then
       begin
         if parseRaw[0] / 1000.0 <= ValuesRaw[i-1] then break;
       end;
     setlength(ValuesRaw,i+1); setlength(ValuesCal,i+1);
     ValuesRaw[i]:= parseRaw[0] / 1000.0;
     ValuesCal[i]:= parseCal[0] / 1000.0;
     inc(i);
   end;
  except
   setlength(ValuesRaw,i);
   setlength(ValuesCal,i);
 end;

 // some ui cosmetics: value values are turned to green
 for j:=0 to i-1 do
  begin
   caledit[j].color:=$a0FFa0;
   rawedit[j].color:=$a0FFa0;
  end;
 for j:=i to 4 do
 begin
   caledit[j].color:=clwindow;
   rawedit[j].color:=clwindow;
  end;

 // send the calibration point to the device
 fct     := TYSensor(functionsList.items.objects[functionsList.itemindex]);
 fctname := functionsList.items[functionsList.itemindex];
 if (fct.isOnline) then fct.calibrateFromPoints(ValuesRaw,ValuesCal);
end;

procedure TForm1.saveBtnClick(Sender: TObject);
 var
  module :Tymodule;
begin
   // saves the device current configuration into flash
   module := TYmodule(devicesList.items.objects[devicesList.itemindex]);
   if module.isOnline() then module.savetoflash();
end;

procedure TForm1.cancelBtnClick(Sender: TObject);
var
  module :Tymodule;
begin
   // reload the device configuration from the flash
   module := TYmodule(devicesList.items.objects[devicesList.itemindex]);
   if module.isOnline() then module.revertfromflash();
   refreshFctUI(true);
end;


end.
