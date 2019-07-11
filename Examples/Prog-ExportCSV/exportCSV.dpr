program inventory;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  yocto_api;

  procedure dumpSensor(sensor : TYSensor);
    var
      dataset     : TYDataset;
      summary,m   : TYMeasure;
      line        : string;
      start,ends  : string;
      fmt         : string;
      progress,i  : integer;
      details     : TYMeasureArray;
    begin
      Writeln('Using DataLogger of ' + sensor.get_friendlyName());
      dataset := sensor.get_recordedData(0, 0);
      Writeln('loading summary... ');
      dataset.loadMore();
      summary := dataset.get_summary();
      fmt := 'dd mmm yyyy hh:nn:ss,zzz';
      DateTimeToString(start, fmt, summary.get_startTimeUTC_asTDateTime);
      DateTimeToString(ends, fmt, summary.get_endTimeUTC_asTDateTime);
      line := Format('from %s to %s : min=%.3f%s avg=%.3f%s  max=%.3f%s',
        [start,ends,
        summary.get_minValue(), sensor.get_unit(),
        summary.get_averageValue(), sensor.get_unit(),
        summary.get_maxValue(), sensor.get_unit()]);
      Writeln(line);
      Write('loading details :   0%');
      repeat
        progress := dataset.loadMore();
        Write(Format(#08#08#08#08'%3d%%', [progress]));
      until progress = 100;
      details := dataset.get_measures();
      for i:=0 to length(details)-1 do
        begin
          m := details[i];
          DateTimeToString(start, fmt, m.get_startTimeUTC_asTDateTime);
          DateTimeToString(ends, fmt, m.get_endTimeUTC_asTDateTime);
          Writeln(Format('from %s to %s : min=%.3f%s avg=%.3f%s  max=%.3f%s',
            [start,ends,
            m.get_minValue(), sensor.get_unit(),
            m.get_averageValue(), sensor.get_unit(),
            m.get_maxValue(), sensor.get_unit()]));
        end;
      dataset.free();
    end;


  
var
  epoch  : TDateTime;
  sensor : TYSensor;
  sensorList : TYSensorArray;
  errmsg : string;
  data   : TYConsolidatedDataSet;
  datarec : TDoubleArray;
  count,idx  : integer;
  line   : string;

begin
  // Setup the API to use local USB devices
  if yRegisterHub('usb', errmsg)<>YAPI_SUCCESS then
  begin
    Writeln('RegisterHub error: '+errmsg);
    exit;
  end;

  // Enumerate all connected sensors
  sensor :=  yFirstSensor();
  if sensor = nil then
    begin
      Writeln('No module connected (check USB cable)');
      exit;
    end;
  count := 0;
  setLength(sensorList, count);
  while sensor <> nil do
    begin
      count := count + 1;
      setLength(sensorList, count);
      sensorList[count-1] := sensor;
      sensor := sensor.nextSensor();
    end;

  //Generate consolidated CSV output for all sensors
  epoch := 25569.0;
  data := TYConsolidatedDataSet.Create(0, 0, sensorList);
  SetLength(datarec,0);
  while (data.nextRecord(datarec) < 100) do
    begin
      DateTimeToString(line, 'yyyy-mm-dd hh:nn:ss.zzz', (datarec[0] / 86400.0) + epoch);
      for idx := 1 to length(datarec)-1 do
        begin
          line := line + Format(';%.3f', [datarec[idx]]);
        end;
      Writeln(line)
    end;
  yFreeAPI();
end. 