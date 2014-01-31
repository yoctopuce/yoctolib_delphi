unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Yocto_api,yjson,yocto_datalogger, Grids, StdCtrls, ComCtrls, ExtCtrls, Buttons,
  ImgList;

type
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    sensorComboBox: TComboBox;
    Label1: TLabel;
    StringGrid1: TStringGrid;
    Timer1: TTimer;
    recordSpeedButton: TSpeedButton;
    StopSpeedButton: TSpeedButton;
    AutoStartCheckBox: TCheckBox;
    ImageList1: TImageList;
    ClearSpeedButton: TSpeedButton;
    Label2: TLabel;
    freqCombo: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure sensorComboBoxChange(Sender: TObject);
    procedure AutoStartCheckBoxClick(Sender: TObject);
    procedure recordSpeedButtonClick(Sender: TObject);
    procedure StopSpeedButtonClick(Sender: TObject);
    procedure ClearSpeedButtonClick(Sender: TObject);
    procedure freqComboChange(Sender: TObject);
  private
    { Private declarations }
    Procedure refreshControls();
    procedure UpdateGridData();

    Procedure ClearGrid();
  public
    { Public declarations }
    Procedure functionsInventory();

  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure devicelistchanged(m:TyModule);
 begin
   // something has changed in the devices list
   // lets refresh it, quick and dirty way.
   form1.functionsInventory();
 end;

 Procedure TForm1.ClearGrid();
  begin
     StringGrid1.ColCount :=0;
     StringGrid1.RowCount := 0;
  end;

Procedure TForm1.functionsInventory();
  var
   sensor             : TYSensor;
   currentSensor      : Tysensor;
   index,i            : integer;
 begin
   // memorize the current selection
   currentsensor := nil;
   if (SensorComboBox.itemindex>=0) then
       currentSensor := TYSensor(SensorComboBox.items.objects[SensorComboBox.itemindex]);

   // update the list, brute force
   SensorComboBox.items.clear;
   sensor := yFirstSensor();
   while sensor<>nil  do
     begin
      SensorComboBox.items.AddObject(sensor.get_friendlyName(),sensor);
      sensor := sensor.nextSensor();
   end;

   // restore previous selection
   if (SensorComboBox.items.count=0) then
    begin
      SensorComboBox.enabled:=false;
      AutoStartCheckBox.enabled:=false;
      ClearGrid();
      refreshControls();
      statusbar1.panels[0].text:='Connect a Yoctopuce sensor device';
    end
    else
    begin
     SensorComboBox.enabled:=true;
     index :=0;
     for i:=0 to SensorComboBox.items.count-1 do
       if  (SensorComboBox.items.objects[i]=currentSensor) then index:=i;

     if (SensorComboBox.items.count =1) then
       statusbar1.panels[0].text:='One Yoctopuce sensor function found'
      else
        statusbar1.panels[0].text:=intToStr(SensorComboBox.items.count)+' Yoctopuce sensor function found';
      SensorComboBox.itemindex:=index;
      if (currentSensor <> SensorComboBox.items.objects[index]) then  // selection has changed
        begin
         refreshControls();
         UpdateGridData();
      end;
    end;
 end;

procedure TForm1.UpdateGridData();
  var
  sensor:Tysensor;
   i:integer;
   details     : TYMeasureArray;
   dataset     : TYDataset;
   progress:integer;
   m   : TYMeasure;
   fmt         : string;
   LastLineLoaded : integer;
   start : string;
   sensorunit:string;
begin

  if (sensorComboBox.items.count>0) then
    statusbar1.panels[0].text:= 'loading Data'
   else
   begin
     statusbar1.panels[0].text:= 'No sensor function  available';
     ClearGrid();
     exit;
   end;
  // disable UI, to make sure noone will change the sensor settings while
  // loading data
  SensorComboBox.enabled:=false;
  freqCombo.enabled:=false;
  recordSpeedButton.enabled:=false;
  StopSpeedButton.enabled:=false;
  ClearSpeedButton.enabled:=false;

  // retreive the sensor
  sensor := Tysensor(SensorComboBox.items.objects[SensorComboBox.itemIndex]);
  dataset  := sensor.get_recordedData(0, 0);

  // some UI preparation
  statusbar1.panels[0].text:= 'Loading summary, please wait';
  application.processMessages;
  fmt := 'dd mmm yyyy hh:nn:ss,zzz';
  StringGrid1.ColCount := 4;
  StringGrid1.RowCount  := 1;
  StringGrid1.cols[0][0]:='Time';
  StringGrid1.cols[1][0]:='Avg';
  StringGrid1.cols[2][0]:='Min';
  StringGrid1.cols[3][0]:='Max';
  StringGrid1.ColWidths[0]:=175;
  StringGrid1.ColWidths[1]:=125;
  StringGrid1.ColWidths[2]:=125;
  StringGrid1.ColWidths[3]:=125;

  // no need to call  get_unit() every  time
  sensorunit := sensor.get_unit();

  // load data
  LastLineLoaded := -1;
  repeat
   progress := dataset.loadMore();
   statusbar1.panels[0].text:= 'loading Data ('+inttostr(round(progress/2))+'%)';
   application.processMessages;
  until progress>=100;

  // how many records ?
  details := dataset.get_measures();
  StringGrid1.RowCount  := length(details)+1;

  // fill up the UI
  for i:=LastLineLoaded+1 to   length(details)-1 do
    begin
        m := details[i];
        DateTimeToString(start, fmt, m.get_startTimeUTC_asTDateTime);
        StringGrid1.cols[0][i+1]:=start;
        StringGrid1.cols[1][i+1]:=Format('%.3f %s', [m.get_averageValue(), sensorunit]);
        StringGrid1.cols[2][i+1]:=Format('%.3f %s', [m.get_minValue(), sensorunit]);
        StringGrid1.cols[3][i+1]:=Format('%.3f %s', [m.get_maxValue(), sensorunit]);
        LastLineLoaded := i;
        if (i mod 1000 =0) then
         begin
           statusbar1.panels[0].text:= 'loading Data ('+inttostr(round(50+50*i/length(details)))+'%), '+inttostr(length(details))+' records found';
           application.processMessages;
         end;

    end;
   SensorComboBox.enabled:=true;
   if (LastLineLoaded>0) then
      StringGrid1.fixedRows  := 1
    else
    begin
       StringGrid1.ColCount  := 1;
       StringGrid1.RowCount  := 1;
       StringGrid1.cols[0][0]:='No data available';
    end;

   // dataset memory need to be freed
   dataset.free();

   //re-enable the UI
   StringGrid1.enableD:=true;
   freqCombo.enabled:=true;
   statusbar1.panels[0].text:='';
   refreshControls();
end;



procedure TForm1.FormCreate(Sender: TObject);
begin

   // we wanna know when device list changes
   statusbar1.panels[0].text:='Connect a Yoctopuce sensor device';
  yRegisterDeviceArrivalCallBack( devicelistchanged);
  yRegisterDeviceRemovalCallBack( devicelistchanged);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
 var
  errmsg:string;
begin
 yUpdateDeviceList(errmsg); // scan for changes
end;


function UnixTimeStampToDateTime(UnixTime: longword): TDateTime;
begin
  Result := 25569 + (UnixTime / 86400);
end;

function DateTimeToUnixTimeStamp(date: TDateTime): longword;
begin
  Result := Round((date - 25569) * 86400);
end;

Procedure  TForm1.refreshControls();
var
  module : TYModule;
  sensor : TYSensor;
  logger:Tydatalogger;
 begin
  if (SensorComboBox.items.count<=0) then
   begin
     recordSpeedButton.enabled:=false;
     ClearSpeedButton.enabled:=false;
     StopSpeedButton.enabled:=true;
     AutoStartCheckBox.enabled:=false;
     freqCombo.enabled:=false;
     exit;
   end;
  freqCombo.enabled:=true;
  sensor := Tysensor(SensorComboBox.items.objects[SensorComboBox.itemIndex]);

  freqCombo.text:=sensor.get_logFrequency();
  module := sensor.get_module();
  logger :=  yFindDataLogger(module.get_serialNumber()+'.dataLogger');
  if (logger.isOnline()) then
    begin

    if logger.get_recording() =  Y_RECORDING_ON then
      begin
       recordSpeedButton.enabled:=false;
       ClearSpeedButton.enabled:=false;
       StopSpeedButton.enabled:=true;
      end
     else
     begin
       recordSpeedButton.enabled:=true;
       ClearSpeedButton.enabled:=true;
       StopSpeedButton.enabled:=false;
     end;

    AutoStartCheckBox.enabled:=true;
    AutoStartCheckBox.onclick:=nil;       // chanigin the value might call the callback
    AutoStartCheckBox.checked:=(logger.get_autoStart()= Y_AUTOSTART_ON);
    AutoStartCheckBox.onclick:=AutoStartCheckBoxClick;
   end;

 end;


procedure TForm1.sensorComboBoxChange(Sender: TObject);

begin
  if (SensorComboBox.itemindex<0) then exit;
  refreshControls();
  UpdateGridData();


end;

procedure TForm1.AutoStartCheckBoxClick(Sender: TObject);
 var
  module : TYModule;
  sensor : TYsensor;
  logger:Tydatalogger;

begin
  if (SensorComboBox.itemindex<0) then exit;
  sensor := Tysensor(SensorComboBox.items.objects[SensorComboBox.itemIndex]);
  module := sensor.get_module();
  logger :=  yFindDataLogger(module.get_serialNumber()+'.dataLogger');
  if (logger.isOnline()) then
   begin
    if (AutoStartCheckBox.checked) then
       logger.set_autoStart( Y_AUTOSTART_ON)
      else
       logger.set_autoStart( Y_AUTOSTART_OFF);

     module.saveToFlash();
   end;
end;


procedure TForm1.recordSpeedButtonClick(Sender: TObject);
var
  module : TYModule;
  sensor : tYsensor;
  logger:Tydatalogger;
begin
  sensor := Tysensor(SensorComboBox.items.objects[SensorComboBox.itemIndex]);
  module := sensor.get_module();
  logger :=  yFindDataLogger(module.get_serialNumber()+'.dataLogger');
  if (logger.isOnline()) then
  begin
    logger.set_timeUTC(DateTimeToUnixTimeStamp(now));
    logger.set_recording(Y_RECORDING_ON);
    recordSpeedButton.enabled:=false;
    ClearSpeedButton.enabled:=false;
    StopSpeedButton.enabled:=true;
   end;
end;

procedure TForm1.StopSpeedButtonClick(Sender: TObject);
var
  module : TYModule;
   sensor : tYsensor;
  logger:Tydatalogger;
begin
  sensor := Tysensor(SensorComboBox.items.objects[SensorComboBox.itemIndex]);
  module := sensor.get_module();
  logger :=  yFindDataLogger(module.get_serialNumber()+'.dataLogger');
  if (logger.isOnline()) then
  begin
    logger.set_recording(Y_RECORDING_OFF);
    recordSpeedButton.enabled:=true;
    StopSpeedButton.enabled:=false;
    ClearSpeedButton.enabled:=true;
    UpdateGridData();
  end;
end;



procedure TForm1.ClearSpeedButtonClick(Sender: TObject);
var
  module : TYModule;
  sensor : tYsensor;
  logger:Tydatalogger;
begin
  if  MessageDlg('Do you really want to clear device datalogger memory?',mtConfirmation, [mbYes,MbNo] , 0) = mrYes then
  begin
    sensor := Tysensor(SensorComboBox.items.objects[SensorComboBox.itemIndex]);
    module := sensor.get_module();
    logger :=  yFindDataLogger(module.get_serialNumber()+'.dataLogger');
    if (logger.isOnline()) then
    begin
      logger.forgetAllDataStreams();
      UpdateGridData();
      
    end;
   end;
end;

var
   errmsg: string;


procedure TForm1.freqComboChange(Sender: TObject);
var
  module : TYModule;
  sensor : tYsensor;
begin
  sensor := Tysensor(SensorComboBox.items.objects[SensorComboBox.itemIndex]);
  sensor.set_logFrequency(freqCombo.text);
  module := sensor.get_module();
  module.saveToFlash();
end;

initialization
  if yRegisterHub('usb', errmsg)<>YAPI_SUCCESS then
    begin
      Application.MessageBox(pchar('Yocto-api init failed: '+errmsg), 'Error', MB_OK);
      application.terminate();
    end;
   yDisableExceptions();

finalization
   yfreeapi();
   
end.
