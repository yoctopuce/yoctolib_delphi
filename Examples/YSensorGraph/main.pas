unit main;

interface

{$IFDEF VER230}
{$DEFINE UNITSCOPE}
{$ENDIF}
{$IFDEF VER240}
{$DEFINE UNITSCOPE}
{$ENDIF}
{$IFDEF VER250}
{$DEFINE UNITSCOPE}
{$ENDIF}
{$IFDEF VER260}
{$DEFINE UNITSCOPE}
{$ENDIF}
{$IFDEF VER270}
{$DEFINE UNITSCOPE}
{$ENDIF}
{$IFDEF VER280}
{$DEFINE UNITSCOPE}
{$ENDIF}
{$IFDEF VER290}
{$DEFINE UNITSCOPE}
{$ENDIF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  {$IFDEF UNITSCOPE}
     VCLTee.TeeProcs, VCLTee.TeEngine, VCLTee.Chart, VCLTee.Series,
  {$ELSE}
     TeeProcs, TeEngine, Chart, Series,
  {$ENDIF}
  ComCtrls, Buttons,yocto_api,Yocto_datalogger;

type
  TForm1 = class(TForm)
    Chart1: TChart;
    Label1: TLabel;
    ComboBox1: TComboBox;
    RecordButton: TSpeedButton;
    PauseButton: TSpeedButton;
    DeleteButton: TSpeedButton;
    StatusBar1: TStatusBar;
    refreshTimer: TTimer;
    InventoryTimer: TTimer;
    Series1: TFastLineSeries;
    ConnectPlz: TLabel;
    loading: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure refreshTimerTimer(Sender: TObject);
    procedure InventoryTimerTimer(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure StatusBar1DrawPanel(StatusBar: TStatusBar;
      Panel: TStatusPanel; const R: TRect);
    procedure DataLoggerButton_Click(Sender: TObject);
    procedure Chart1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    loadprogress : integer;
    FirstPointDate, LastPointDate :double;
    procedure  deviceRemoval( m : TYModule);
    procedure  deviceArrival( m : TYModule);
    procedure  newSensorValue(f: TYFunction; v:TYMeasure);
    procedure  refreshDatloggerButton(s:TYSensor);
    procedure  setSensorCount();
    procedure  clearGraph();
    function   getSelectedSensor() : TYsensor;
    procedure  setGraphScale();

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

// automatically called each time a new yoctopuce device is plugged
procedure  deviceArrivalCallBack( m : TYModule);
 begin
  form1.deviceArrival(m);
 end;

// automatically called each time a new yoctopuce device is unplugged
procedure  deviceRemovalCallBack( m : TYModule);
 begin
  form1.deviceRemoval(m);
 end;

// automatically called on a regular basis with sensor value
procedure  newSensorValueCallback(f: TYSensor; v:TYMeasure);
 begin
   form1.newSensorValue(f,v);
 end;

function UnixTimeStampToDateTime(UnixTime: double): double;
begin
  Result := 25569 + (UnixTime / 86400);
end;

procedure  Tform1.deviceArrival( m : TYModule);
  var
    s: TYsensor ;
    index: integer;
    previousIndex :integer;
   begin
   // new device just arrived, lets enumerate all sensors and
   // add the one missing to the combobox
   previousIndex :=   combobox1.itemIndex;
   s := YFirstSensor();
   while (s <> nil) do
     begin
      if (comboBox1.Items.IndexOfObject(s)<0)    then
        begin
          index :=  comboBox1.Items.Add(s.get_friendlyName());
          comboBox1.Items.objects[index] := s;
        end;
        s := s.nextSensor();
     end;
   comboBox1.Enabled := comboBox1.Items.Count>0;
   if ((comboBox1.ItemIndex<0) and (comboBox1.Items.Count>0))  then
       comboBox1.ItemIndex:=0;
   setSensorCount();

   if (previousIndex<>combobox1.itemIndex) then ComboBox1Change(nil);
end;

procedure Tform1.deviceRemoval( m : TYModule);
   var
    I :integer;
   begin
     // a device vas just removed, lets remove the offline sensors
     // from the combo box
     for  i := comboBox1.Items.Count - 1 downto  0 do
     if not TYSensor(comboBox1.Items.objects[i]).isOnline() then
         comboBox1.Items.Delete(i);

     setSensorCount();
 end;

procedure  Tform1.newSensorValue(f: TYFunction ; v:TYMeasure);
  var  t:double;
begin
  t := UnixTimeStampToDateTime(v.get_endTimeUTC());
  chart1.Series[0].AddXY(t,v.get_averageValue());
  if (FirstPointDate<0)  then FirstPointDate:=t ;
  LastPointDate := t;
  setGraphScale()
end;

 // update the UI according to the sensors count
procedure Tform1.setSensorCount() ;
  begin
   if (comboBox1.Items.Count <= 0) then StatusBar1.panels[0].Text := 'No sensor found, check USB cable'
   else if (comboBox1.Items.Count =1)  then StatusBar1.panels[0].Text := 'One sensor found'
   else  StatusBar1.panels[0].Text :=  intToStr(comboBox1.Items.Count)+' sensors found';
   if (comboBox1.Items.Count<=0) then chart1.Visible := false;
   ConnectPlz.Visible := comboBox1.Items.Count <= 0;
   Application.processMessages();
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   // we wanna know when device list changes
  YRegisterDeviceArrivalCallback(deviceArrivalcallback);
  YRegisterDeviceRemovalCallback(deviceRemovalcallback);
  InventoryTimer.Interval := 500;
  refreshTimer.Interval   := 500;
  Chart1.BottomAxis.ExactDateTime := False;
  Chart1.BottomAxis.DateTimeFormat := 'nn:ss';
  setSensorCount();
  Chart1.color := clBtnFace;
end;

procedure TForm1.refreshTimerTimer(Sender: TObject);
 var
   errmsg : string;
begin
  YHandleEvents(errmsg);
end;

procedure TForm1.InventoryTimerTimer(Sender: TObject);
 var
   errmsg : string;
begin
   YUpdateDeviceList(errmsg);
end;

procedure TForm1.clearGraph();
 begin
   Chart1.SeriesList[0].clear();
 end;

 // returns the sensor selected in the combobox
function TForm1.getSelectedSensor() : TYsensor;
  var
     index :integer;
  begin
     index := comboBox1.ItemIndex;
     if (index<0)  then
       begin
         getSelectedSensor :=  nil;
         exit;
       end;
     getSelectedSensor :=  TYSensor(comboBox1.Items.objects[index]);
  end;

       // update the datalogger control buttons
 procedure Tform1.refreshDatloggerButton(s:TYSensor);
 var
  m : TYModule;
  dtl : TYDataLogger;
 begin
   if (s <> nil) then
     begin
        m := s.get_module();  // get the module harboring the sensor
        dtl := YFindDataLogger(m.get_serialNumber() + '.dataLogger');
        if (dtl.isOnline())  then
          begin
           if (dtl.get_recording() = Y_RECORDING_ON) then
              begin
                RecordButton.Enabled := false;
                PauseButton.Enabled  := true;
                DeleteButton.Enabled := false;
                exit;
              end
              else
              begin
                RecordButton.Enabled := true;
                PauseButton.Enabled := false;
                DeleteButton.Enabled := true;
                exit;
               end;
           end;
         end;
      RecordButton.Enabled := false;
      PauseButton.Enabled  := false;
      DeleteButton.Enabled := false;
    end;

// update the date labels format according to graph length
procedure TForm1.setGraphScale();
  var
   count : integer;
   total : double;
  begin
   count := chart1.Series[0].Xvalues.count;
   if (count > 0)  then
   begin
       total := LastPointDate - FirstPointDate;
       if (total < 180) then Chart1.BottomAxis.DateTimeFormat := 'h:nn:ss'
       else if (total < 3600) then Chart1.BottomAxis.DateTimeFormat := 'h:nn'
       else if (total < 3600 * 24) then Chart1.BottomAxis.DateTimeFormat := 'h:nn'
       else if (total < 3600 * 24 * 7) then Chart1.BottomAxis.DateTimeFormat := 'ddd H'
       else if (total < 3600 * 24 * 30) then Chart1.BottomAxis.DateTimeFormat := 'dd-mm'
       else Chart1.BottomAxis.DateTimeFormat := 'mmm';
   end  else Chart1.BottomAxis.DateTimeFormat :='nn:ss';
 end;

// Datalogger buttons handling
procedure TForm1.DataLoggerButton_Click(Sender: TObject);
var
   s   : TYSensor;
   m   : TYModule;
   dtl : TYDataLogger;
begin
 s := getSelectedSensor();
 if (s <> nil) then
   begin
       m := s.get_module();  // get the module harboring the sensor
       dtl := YFindDataLogger(m.get_serialNumber() + '.dataLogger');
       if (dtl.isOnline()) then
          begin
            if (sender=RecordButton) then dtl.set_recording(Y_RECORDING_ON);
            if (sender=PauseButton)  then dtl.set_recording(Y_RECORDING_OFF);
            if (sender = DeleteButton) then
                begin
                    dtl.set_recording(Y_RECORDING_OFF);
                    dtl.forgetAllDataStreams();
                    clearGraph();
                end;
           end;
      end;
   refreshDatloggerButton(s);
  end;

// the core function :  load data from datalogger to send it to the graph
procedure TForm1.ComboBox1Change(Sender: TObject);
var
  I,index   : integer;
  data      : TYDataSet ;
  alldata   : TYMeasureArray;
  s         : TYSensor;
begin
  // lets hide the graph wgile updating
  chart1.Visible    := false;
  comboBox1.Enabled := false;
  alldata           := nil;
  // remove any previous timed report call back
  for  i := 0 to comboBox1.Items.Count-1 do
     TYSensor(comboBox1.Items.objects[i]).registerTimedReportCallback(nil);

   index := comboBox1.itemIndex;
  if (index >= 0) then clearGraph();

  s := getSelectedSensor();
  if (s <> nil)  then
    begin
      FirstPointDate :=-1;
      LastPointDate  :=-1;
      // some ui control
      loading.Visible:=true;
      //refreshDatloggerButton(nil);
      //progressBar.Visible := true;
      Statusbar1.panels[0].Text := 'Loading data from datalogger...';
      for i := 0 to  100 do Application.processmessages(); // makes sure the UI changes are repainted

      // load data from datalogger
      data := s.get_recordedData(0, 0);
      loadprogress := data.loadMore();
      while (loadprogress < 100)  do
        begin
          // progressBar.Value = progress;
          statusBar1.repaint();
          Application.processMessages();
          loadprogress := data.loadMore();
        end;

       // sets the unit (because ° is not a ASCII-128  character, Yoctopuce temperature
       // sensors report unit as 'C , so we fix it).
       chart1.LeftAxis.Title.caption := StringReplace(s.get_unit(),'''C','°C',[rfReplaceAll, rfIgnoreCase]);

       // send the data to the graph
       alldata := data.get_measures();
       for  i := 0 to  length(alldata)-1 do
            chart1.Series[0].AddXY(UnixTimeStampToDateTime(alldata[i].get_endTimeUTC()), alldata[i].get_averageValue());

      data.free();

      // restore UI
      comboBox1.Enabled:=true;

      // reste progress bar
      loadprogress := 0;
      statusBar1.repaint();
      setGraphScale;
      setSensorCount();
      s.set_reportFrequency('100/s');
      s.registerTimedReportCallback(newSensorValueCallback);
      loading.Visible := false;
      chart1.Visible:=true;
      refreshDatloggerButton(s);

     end;
    chart1.Visible    := true;

end;

// draw the progess bar in the statusBar
procedure TForm1.StatusBar1DrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const R: TRect);
  var
     w,middle:integer;
  begin
  if  Panel.index=1 then
   begin
     w := r.right - r.left;
     middle := r.left+round( w* loadprogress / 100);
     StatusBar.Canvas.brush.color:=clNavy;
     if (loadProgress>0)   then StatusBar.Canvas.FillRect(Rect(r.left,r.top,middle,r.bottom)) ;
      StatusBar.Canvas.brush.color:= clBtnFace;
     if (loadProgress<100) then StatusBar.Canvas.FillRect(Rect(middle+1,r.top,r.right,r.bottom)) ;
   end;
end;

// cheap zoom-out
procedure TForm1.Chart1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if  button=mbmiddle  then  Chart1.undoZoom();

  if  button=mbRight then
     Chart1.ZoomRect( Rect( -10,-10,Chart1.Width+50,Chart1.Height+50 ) );
end;


end.
