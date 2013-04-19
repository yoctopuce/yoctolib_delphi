
//
//  Generic UI to calibrate Yocto sensors using integrated
//  1-5 points linear interpolation. Most of the code is UI
//  handling, but the key function is CalibrationChange()
//

unit mainform;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,yocto_api,yocto_temperature,yocto_pressure,yocto_lightsensor,yocto_carbondioxide,
  yocto_humidity,yocto_voltage,yocto_current,ExtCtrls, ComCtrls,math;

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
    procedure  DisplayTemperature(fct:TYTemperature);
    procedure  DisplayHumidity(fct:TYhumidity);
    procedure  DisplayPressure(fct:TYPressure);
    procedure  DisplayLightSensor(fct:TYlightSensor);
    procedure  DisplayCarbonDioxide(fct:TYcarbondioxide);
    procedure  DisplayVoltage(fct:TYvoltage);
    procedure  DisplayCurrent(fct:TyCurrent);
    procedure  DisplayTemperatureCalPoints(fct:TYTemperature);
    procedure  DisplayHumidityCalPoints(fct:TYhumidity);
    procedure  DisplayPressureCalPoints(fct:TYPressure);
    procedure  DisplayLightSensorCalPoints(fct:TYlightSensor);
    procedure  DisplayCarbonDioxideCalPoints(fct:TYcarbondioxide);
    procedure  DisplayVoltageCalPoints(fct:TYvoltage);
    procedure  DisplayCurrentCalPoints(fct:TyCurrent);
    procedure  displayValue(value,rawvalue:double;resolution:double;valunit:string);
    procedure DisplayCalPoints(ValuesRaw,ValuesCal : floatArr;resolution:double ) ;
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
  mustrefresh :=  index =  devicesList.itemindex;


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
  currentDevice  : Tymodule;
  fctount,i      : integer;
  fct            : TYfunction;
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
  currentDevice := TYmodule(devicesList.items.objects[devicesList.itemindex]);

  // populate the second drop down
  if (currentDevice.isOnline()) then
   begin
     // device capabilities inventory
     fctount := currentDevice.FunctionCount();
     for i:=0 to  fctount-1 do
      begin
       fctName := currentDevice.functionId(i);
       fctFullName := currentDevice.get_serialNumber() + '.' + fctName;
       fct:=nil;
       // We have to have handle each sensor type independtly, (sorry about that)
       if  (pos ('temperature',fctName)=1)  then fct:=TYfunction(YfindTemperature(fctFullName));
       if  (pos ('humidity',fctName)=1)     then fct:=TYfunction(YfindHumidity(fctFullName));
       if  (pos ('pressure',fctName)=1)     then fct:=TYfunction(YfindPressure(fctFullName));
       if  (pos ('lightSensor',fctName)=1)  then fct:=TYfunction(YfindLightSensor(fctFullName));
       if  (pos ('carbonDiodyde',fctName)=1)then fct:=TYfunction(YfindCarbonDioxide(fctFullName));
       if  (pos ('voltage',fctName)=1)      then fct:=TYfunction(YfindVoltage(fctFullName));
       if  (pos ('current',fctName)=1)      then fct:=TYfunction(YfindCurrent(fctFullName));

       // add the function in the second drop down
       if (fct<>nil) then functionsList.items.addObject(fctName,fct);
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
 fct:Tyfunction;
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

  fct     := TYfunction(functionsList.items.objects[functionsList.itemindex]);
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
     if  (pos ('temperature',fctName)=1)   then DisplayTemperatureCalPoints(TYTemperature(fct));
     if  (pos ('pressure',fctName)=1)      then DisplayPressureCalPoints(TYPressure(fct));
     if  (pos ('humidity',fctName)=1)      then DisplayHumidityCalPoints(TYHumidity(fct));
     if  (pos ('lightSensor',fctName)=1)   then DisplayLightSensorCalPoints(TYlightSensor(fct));
     if  (pos ('carbonDioxide',fctName)=1) then DisplayCarbonDioxideCalPoints(TYcarbondioxide(fct));
     if  (pos ('voltage',fctName)=1)       then DisplayVoltageCalPoints(TYvoltage(fct));
     if  (pos ('current',fctName)=1)       then DisplayCurrentCalPoints(TyCurrent(fct));
   end;

  if (fct.isOnline) then
   begin
    if  (pos ('temperature',fctName)=1)  then DisplayTemperature(TYTemperature(fct));
    if  (pos ('pressure',fctName)=1)     then DisplayPressure(TYPressure(fct));
    if  (pos ('humidity',fctName)=1)     then DisplayHumidity(TYhumidity(fct));
    if  (pos ('lightSensor',fctName)=1)  then DisplayLightSensor(TYlightSensor(fct));
    if  (pos ('carbonDioxide',fctName)=1)then DisplayCarbonDioxide(TYcarbondioxide(fct));
    if  (pos ('voltage',fctName)=1)      then DisplayVoltage(TYvoltage(fct));
    if  (pos ('current',fctName)=1)      then DisplayCurrent(TyCurrent(fct));
   end;
 end;

 procedure Tform1.displayValue(value,rawvalue:double;resolution:double;valunit:string);
   begin
    // displays the sensor value on the ui
    ValueDisplayUnits.caption:=valunit;
     resolution:=0.1;
    if resolution<>Y_RESOLUTION_INVALID  then
     begin
      // if resolution is available on the device the use it to  round the value
      resolution := power(10,trunc(log10(resolution)));
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

 procedure Tform1.DisplayCalPoints(ValuesRaw,ValuesCal : floatArr;resolution:double ) ;
 var i:integer;
  begin
  // little trick: if resolution is not available on the device, the
  // calibration in not available either
  if   resolution=Y_RESOLUTION_INVALID then
   begin
     EnableCalibrationUI(false);
     unsupported_warning.visible:=true;
     exit;
  end;

  // display the calibration points
  unsupported_warning.visible:=false;
  for i:=0 to high(ValuesRaw) do
   begin
     rawedit[i].text:=floattostr(ValuesRaw[i]);
     caledit[i].text:=floattostr(Valuescal[i]);
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
 ValuesRaw : floatArr;
 ValuesCal : floatArr;
 fct       : tyfunction;
 stop      : boolean;
 i,j :integer;
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
  stop:=false;
  try
  while  (i<5) and (caledit[i].text<>'') and (rawedit[i].text<>'') and not(Stop) do
   begin
     setlength(ValuesRaw,i+1); setlength(ValuesCal,i+1);
     ValuesCal[i]:=strtofloat(caledit[i].text);
     ValuesRaw[i]:=strtofloat(rawedit[i].text);
     if (i>0) then
       if ValuesRaw[i]<= ValuesRaw[i-1] then
        begin
          stop:=true;
          setlength(ValuesRaw,i);
          setlength(ValuesCal,i);
          dec(i);
       end;
     inc(i);
   end
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
 fct     := TYfunction(functionsList.items.objects[functionsList.itemindex]);
 fctname := functionsList.items[functionsList.itemindex];

 if (fct.isOnline) then
   begin
    if  (pos ('temperature',fctName)=1)   then TYTemperature(fct).calibrateFromPoints(ValuesRaw,ValuesCal);
    if  (pos ('pressure',fctName)=1)      then TYhumidity(fct).calibrateFromPoints(ValuesRaw,ValuesCal);
    if  (pos ('lightSensor',fctName)=1)   then TYlightSensor(fct).calibrateFromPoints(ValuesRaw,ValuesCal);
    if  (pos ('carbonDioxide',fctName)=1) then TYcarbondioxide(fct).calibrateFromPoints(ValuesRaw,ValuesCal);
    if  (pos ('voltage',fctName)=1)       then TYvoltage(fct).calibrateFromPoints(ValuesRaw,ValuesCal);
    if  (pos ('current',fctName)=1)       then TYCurrent(fct).calibrateFromPoints(ValuesRaw,ValuesCal);
    if  (pos ('humidity',fctName)=1)      then TYHumidity(fct).calibrateFromPoints(ValuesRaw,ValuesCal);

   end;


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

// this the weak point of the API, methods get_currentValue,
// get_resolution,  get_unit etc... are present in all sensor classes, but
// are  not inherited from the parent class (to keep the object model
// simple) we have to handle them independtly for each sensor type.

 procedure  TForm1.DisplayTemperature(fct:TYTemperature);
   begin displayValue(fct.get_currentValue(),fct.get_currentRawValue(),fct.get_resolution(),fct.get_unit());end;

 procedure  TForm1.DisplayPressure(fct:TYPressure);
   begin displayValue(fct.get_currentValue(),fct.get_currentRawValue(),fct.get_resolution(),fct.get_unit());end;


 procedure  TForm1.DisplayHumidity(fct:TYhumidity);
   begin displayValue(fct.get_currentValue(),fct.get_currentRawValue(),fct.get_resolution(),fct.get_unit());end;

 procedure  TForm1.DisplayLightSensor(fct:TYlightSensor);
   begin displayValue(fct.get_currentValue(),fct.get_currentRawValue(),fct.get_resolution(),fct.get_unit());end;

 procedure  TForm1.DisplayCarbonDioxide(fct:TYcarbondioxide);
   begin displayValue(fct.get_currentValue(),fct.get_currentRawValue(),fct.get_resolution(),fct.get_unit());end;

 procedure  TForm1.DisplayVoltage(fct:TYvoltage);
   begin displayValue(fct.get_currentValue(),fct.get_currentRawValue(),fct.get_resolution(),fct.get_unit());end;

 procedure  TForm1.DisplayCurrent(fct:TyCurrent);
   begin displayValue(fct.get_currentValue(),fct.get_currentRawValue(),fct.get_resolution(),fct.get_unit());end;



 procedure   TForm1.DisplayTemperatureCalPoints(fct:TYTemperature);
 var  ValuesRaw,ValuesCal : floatArr;
 begin
    fct.loadCalibrationPoints(ValuesRaw,ValuesCal);
    DisplayCalPoints(ValuesRaw,ValuesCal,fct.get_resolution());
  end;

 procedure   TForm1.DisplayPressureCalPoints(fct:TYPressure);
 var  ValuesRaw,ValuesCal : floatArr;
 begin
    fct.loadCalibrationPoints(ValuesRaw,ValuesCal);
    DisplayCalPoints(ValuesRaw,ValuesCal,fct.get_resolution());
  end;

 procedure  TForm1.DisplayHumidityCalPoints(fct:TYhumidity);
 var  ValuesRaw,ValuesCal : floatArr;
 begin
    fct.loadCalibrationPoints(ValuesRaw,ValuesCal);
    DisplayCalPoints(ValuesRaw,ValuesCal,fct.get_resolution());
  end;

 procedure  TForm1.DisplayLightSensorCalPoints(fct:TYlightSensor);
 var  ValuesRaw,ValuesCal : floatArr;
 begin
    fct.loadCalibrationPoints(ValuesRaw,ValuesCal);
    DisplayCalPoints(ValuesRaw,ValuesCal,fct.get_resolution());
  end;

 procedure  TForm1.DisplayCarbonDioxideCalPoints(fct:TYcarbondioxide);
 var  ValuesRaw,ValuesCal : floatArr;
 begin
    fct.loadCalibrationPoints(ValuesRaw,ValuesCal);
    DisplayCalPoints(ValuesRaw,ValuesCal,fct.get_resolution());
  end;

 procedure  TForm1.DisplayVoltageCalPoints(fct:TYvoltage);
 var  ValuesRaw,ValuesCal : floatArr;
 begin
    fct.loadCalibrationPoints(ValuesRaw,ValuesCal);
    DisplayCalPoints(ValuesRaw,ValuesCal,fct.get_resolution());
  end;

 procedure  TForm1.DisplayCurrentCalPoints(fct:TyCurrent);
 var  ValuesRaw,ValuesCal : floatArr;
 begin
    fct.loadCalibrationPoints(ValuesRaw,ValuesCal);
    DisplayCalPoints(ValuesRaw,ValuesCal,fct.get_resolution());
  end;


end.
