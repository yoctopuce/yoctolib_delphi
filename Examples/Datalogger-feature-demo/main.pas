unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Yocto_api, yocto_datalogger, Grids, StdCtrls, ComCtrls, ExtCtrls, Buttons,
  ImgList;

type
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    deviceComboBox: TComboBox;
    Label1: TLabel;
    StringGrid1: TStringGrid;
    Timer1: TTimer;
    Label2: TLabel;
    StreamComboBox: TComboBox;
    recordSpeedButton: TSpeedButton;
    StopSpeedButton: TSpeedButton;
    AutoStartCheckBox: TCheckBox;
    ImageList1: TImageList;
    ClearSpeedButton: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure StreamComboBoxChange(Sender: TObject);
    procedure deviceComboBoxChange(Sender: TObject);
    procedure AutoStartCheckBoxClick(Sender: TObject);
    procedure recordSpeedButtonClick(Sender: TObject);
    procedure StopSpeedButtonClick(Sender: TObject);
    procedure ClearSpeedButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    Procedure refreshControls();
    Procedure UpdateStreamList();
    procedure UpdateStreamData();
    procedure ClearStreamComboBox();
    Procedure ClearGrid();
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

 Procedure TForm1.ClearGrid();
  begin
     StringGrid1.ColCount :=0;
     StringGrid1.RowCount := 0;
  end;

Procedure TForm1.modulesInventory();
  var
   module             : TYModule;
   name,lname         : string;
   currentModule      : Tymodule;
   index,i            : integer;
   fctcount           : integer;
   containsDatalogger : boolean;
   fctHardwareName    : string;
 begin
   // memorize the current selection
   currentModule := nil;
   if (DeviceComboBox.itemindex>=0) then
       currentModule := TYModule(DeviceComboBox.items.objects[DeviceComboBox.itemindex]);

   // update the list, brute force
   DeviceComboBox.items.clear;
   module := yFirstModule();
   while module<>nil  do
   begin
     // does the module contains a datalogger feature ?
     name               :=  module.get_serialNumber();
     fctcount           :=  module.functionCount();
     containsDatalogger := false;
     for i:=0 to  fctcount-1 do
      begin
       fctHardwareName := module.functionid(i);
       if (fctHardwareName='dataLogger')  then
         containsDatalogger := true;      // yes
      end;

     if containsDatalogger then
      begin
       lname :=  module.get_logicalName();
       if (lname<>'') then  name:=name+' ('+lname+')';
       DeviceComboBox.items.AddObject(name,module);
      end;
      module := module.nextModule();
   end;

   // restore previous selection
   if (DeviceComboBox.items.count=0) then
    begin
      DeviceComboBox.enabled:=false;
      AutoStartCheckBox.enabled:=false;
      ClearStreamComboBox();
      StreamComboBox.enabled:=false;
      ClearGrid();
      refreshControls();
      statusbar1.panels[0].text:='Connect a Yoctopuce sensor device';

    end
    else
    begin
     DeviceComboBox.enabled:=true;

     index :=0;
     for i:=0 to DeviceComboBox.items.count-1 do
       if  (DeviceComboBox.items.objects[i]=currentModule) then index:=i;

     if (DeviceComboBox.items.count =1) then
       statusbar1.panels[0].text:='One Yoctopuce sensor device connected'
      else
        statusbar1.panels[0].text:=intToStr(DeviceComboBox.items.count)+' Yoctopuce sensor devices connected';
      DeviceComboBox.itemindex:=index;
      if (currentModule <> DeviceComboBox.items.objects[index]) then  // selection has changed
      UpdateStreamList();
      refreshControls();
    end;
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

Procedure TForm1.UpdateStreamList();
 var
   stream : TYdataStream ;
   m:tymodule;
   logger:Tydatalogger;
   datastreams:Tlist;
   i:integer;
   description:string;
   startTimeUTC:longword;
   count:integer;

 begin
  application.processMessages();
  ClearStreamComboBox();
  m   := Tymodule(DeviceComboBox.items.objects[DeviceComboBox.itemIndex]);
  logger :=  yFindDataLogger(m.get_serialNumber()+'.dataLogger');
  if (logger.isOnline()) then
  begin
    statusbar1.panels[0].text:= 'loading streams list';
    application.processMessages();
    datastreams:=tlist.create;
    if (logger.get_dataStreams(datastreams)=YAPI_SUCCESS) then
      begin
        count:=  datastreams.count;
        if (datastreams.count>0) then
          begin
           for i:=0 to count-1 do
             begin
               application.processMessages();
               stream       := TYdataStream(datastreams.items[i]);
               description  := 'Run #'+inttostr(stream.get_runIndex())+': ';
               startTimeUTC := stream. get_startTimeUTC() ;
               if (startTimeUTC>0) then description:=description+DateTimeToStr(UnixTimeStampToDateTime(startTimeUTC))
                                   else   description:=description+'Device start + '+intToStr(stream. get_startTime())+' sec';
               StreamComboBox.items.add(description);
               StreamComboBox.items.objects[StreamComboBox.items.count-1]:= stream;
             end;
             StreamComboBox.enabled:=true;
             StreamComboBox.itemindex:=0;
             statusbar1.panels[0].text:= '';
          end
        else
          begin
           StreamComboBox.enableD:=false;
           statusbar1.panels[0].text:='No data stream available on this device';
          end;
      end
    else statusbar1.panels[0].text:= logger.errMessage();
    datastreams.free();
   end ;
   UpdateStreamData();
 end;

procedure TForm1.UpdateStreamData();
  var
   m:tymodule;
   logger:Tydatalogger;
   rowcount,colCount :integer;
   startTimeUTC,starttime :longword;
   colnames:Tstringlist;
   i,j:integer;
   stream : TYdataStream ;
   data : TYDataLoggerRawData ;
   interval : integer;
   description :string;
begin
  data :=nil;
  if (StreamComboBox.items.count>0) then
    statusbar1.panels[0].text:= 'loading stream #'+intToStr(StreamComboBox.itemindex+1)
   else
    statusbar1.panels[0].text:= 'No steam available';

  StringGrid1.enableD:=false;
  application.processMessages();
  if (StreamComboBox.items.count<=0)  then
   begin
     ClearGrid();
     exit;
   end;
  m   := Tymodule(DeviceComboBox.items.objects[DeviceComboBox.itemIndex]);
  logger :=  yFindDataLogger(m.get_serialNumber()+'.dataLogger');
  if (logger.isOnline()) then
   begin
    stream   :=  TYdataStream(StreamComboBox.items.objects[StreamComboBox.itemindex]);
    stream.loadStream(); // force a refresh.
    data := stream.get_dataRows();
    colnames :=  stream.get_columnNames();
    colCount :=  colnames.count;
    rowcount :=  stream.get_rowCount();
    StringGrid1.ColCount := colCount+1;
    StringGrid1.RowCount := rowcount+1;
    if (colCount>1) then StringGrid1.fixedcols :=1;
    if (rowCount>1) then StringGrid1.fixedrows :=1;
    StringGrid1.ColWidths[0]:=150;
    interval := stream.get_dataSamplesInterval() ;
    startTimeUTC := stream. get_startTimeUTC() ;
    starttime    := stream. get_startTime() ;
    for i:=0 to colnames.count-1 do
        StringGrid1.cols[i+1][0]:=colnames.strings[i];
    for j:=0 to RowCount-1 do
     begin
      if (startTimeUTC>0) then description:=DateTimeToStr(UnixTimeStampToDateTime(integer(startTimeUTC)+interval*j))
                          else description:='Device start + '+intToStr(integer(starttime)+interval*j)+' sec';
      StringGrid1.cols[0][j+1] := description;
      for i:=0 to colCount-1 do
         StringGrid1.cols[i+1][j+1]:=  FloatToStrF(data[j][i],ffFixed,15,1);
     end;
   end;
   StringGrid1.enableD:=true;
   statusbar1.panels[0].text:='';
end;

procedure TForm1.StreamComboBoxChange(Sender: TObject);
begin
  UpdateStreamData();
end;

Procedure  TForm1.refreshControls();
var
  module : TYModule;
  logger:Tydatalogger;
 begin
  if (DeviceComboBox.items.count<=0) then
   begin
     recordSpeedButton.enabled:=false;
     ClearSpeedButton.enabled:=false;
     StopSpeedButton.enabled:=true;
     AutoStartCheckBox.enabled:=false;
     exit;
   end;

  module := TYModule(DeviceComboBox.items.objects[DeviceComboBox.itemindex]);
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
    UpdateStreamData();
    AutoStartCheckBox.enabled:=true;
    AutoStartCheckBox.onclick:=nil;       // chanigin the value might call the callback
    AutoStartCheckBox.checked:=(logger.get_autoStart()= Y_AUTOSTART_ON);
    AutoStartCheckBox.onclick:=AutoStartCheckBoxClick;
   end;

 end;

procedure TForm1.deviceComboBoxChange(Sender: TObject);

begin
  if (DeviceComboBox.itemindex<0) then exit;
  UpdateStreamList();
   refreshControls();

end;

procedure TForm1.AutoStartCheckBoxClick(Sender: TObject);
 var
  module : TYModule;
  logger:Tydatalogger;

begin
  if (DeviceComboBox.itemindex<0) then exit;
  module := Tymodule(DeviceComboBox.items.objects[DeviceComboBox.itemIndex]);
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
  logger:Tydatalogger;
begin
  module := Tymodule(DeviceComboBox.items.objects[DeviceComboBox.itemIndex]);
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
  logger:Tydatalogger;
begin
  module := Tymodule(DeviceComboBox.items.objects[DeviceComboBox.itemIndex]);
  logger :=  yFindDataLogger(module.get_serialNumber()+'.dataLogger');
  if (logger.isOnline()) then
  begin
    logger.set_recording(Y_RECORDING_OFF);
    recordSpeedButton.enabled:=true;
    StopSpeedButton.enabled:=false;
    ClearSpeedButton.enabled:=true;
    UpdateStreamList();
    StreamComboBox.itemIndex:=  StreamComboBox.items.count-1;
    UpdateStreamData();
  end;
end;

Procedure  TForm1.ClearStreamComboBox();
  var
   i:integer;
   stream : TYdataStream ;
 begin
   for i:=0 to  StreamComboBox.items.count-1 do
    begin
       Stream   :=  TYdataStream(StreamComboBox.items.objects[i]);
       StreamComboBox.items.objects[i] :=nil;
       Stream.free;
    end;
   StreamComboBox.clear();
 end;

procedure TForm1.ClearSpeedButtonClick(Sender: TObject);
var
  module : TYModule;
  logger:Tydatalogger;
begin
  if  MessageDlg('Do you really want to clear device datalogger memory?',mtConfirmation, [mbYes,MbNo] , 0) = mrYes then
  begin
    module := Tymodule(DeviceComboBox.items.objects[DeviceComboBox.itemIndex]);
    logger :=  yFindDataLogger(module.get_serialNumber()+'.dataLogger');
    if (logger.isOnline()) then
    begin
      logger.forgetAllDataStreams();
      UpdateStreamList();
    end;
   end;
end;

var
   errmsg: string;


procedure TForm1.FormDestroy(Sender: TObject);
begin
 ClearStreamComboBox();
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
