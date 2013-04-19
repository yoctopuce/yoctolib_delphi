program demo;
{$APPTYPE CONSOLE}
uses
  SysUtils,yocto_api,yocto_anButton,yocto_temperature,yocto_lightsensor;

  Procedure anButtonChangeCallBack(fct :TYAnButton; value:string);
   begin
    writeln('Position change         : ' + fct.toString() + ' = ' + value);
   end;

  Procedure temperatureChangeCallBack(fct :TYTemperature; value:string);
   begin
    writeln('Temperature change      : ' + fct.toString() + ' = ' + value + ''+chr(248)+'C');
   end;

  Procedure lightSensorChangeCallBack(fct :TYLightSensor; value:string);
   begin
    writeln('Light change            : ' + fct.toString() + ' = ' + value + 'lx');
   end;

  Procedure deviceArrival(m:Tymodule);
   var
     fctName, fctFullName :string;
     fctcount,i :integer;
     bt :  TYAnButton;
     t  : TYtemperature;
     l  : TYLightSensor;

   begin
     writeln('Device arrival          : ' + m.ToString());
     fctcount := m.functionCount();
     for  i := 0 to fctcount-1 do
      begin
        fctName := m.functionId(i);
        fctFullName := m.get_serialNumber() + '.' + fctName;

        // register call back for anbuttons
        if (pos ('anButton',fctName)=1) then
        begin
          bt := YFindAnButton(fctFullName);
          if(bt.isOnline()) then  bt.registerValueCallback(anButtonChangeCallBack);
          writeln('Callback registered for : ' + fctFullName);
        end;

        // register call back for temperature sensors
        if (pos ('temperature',fctName)=1) then
        begin
          t := YFindTemperature(fctFullName);
          if(t.isOnline()) then t.registerValueCallback(temperatureChangeCallBack);
          writeln('Callback registered for : ' + fctFullName);
        end;

         // register call back for light sensors
        if (pos ('lightSensor',fctName)=1) then
        begin
          l := YFindLightSensor(fctFullName);
          if (l.isOnline()) then l.registerValueCallback(lightSensorChangeCallBack);
          writeln('Callback registered for : ' + fctFullName);
        end;

         // and so on for other sensor type.....
      end;
   end;

   procedure deviceRemoval( m: TYmodule);
   begin
      Writeln('Device removal          : ' + m.get_serialNumber());
   end;

var
   errmsg:string;
begin

 if (yRegisterHub('usb',  errmsg) <> YAPI_SUCCESS) then
   begin
     WriteLn('RegisterHub error : ' + errmsg);
     halt;
   end;

  yRegisterDeviceArrivalCallback(deviceArrival);
  yRegisterDeviceRemovalCallback(deviceRemoval);


  WriteLn('Hit Ctrl-C to Stop ');

  while(true) do
   begin
      yUpdateDeviceList( errmsg); // traps plug/unplug events
      ySleep(500, errmsg);   // traps others events
   end;

end.