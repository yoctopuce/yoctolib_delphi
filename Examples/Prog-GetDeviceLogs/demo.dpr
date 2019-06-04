program demo;
{$APPTYPE CONSOLE}
uses
  SysUtils,yocto_api,yocto_anButton;

  Procedure logfun(m:TYModule; st:string);
   begin
    writeln(m.get_serialNumber() + ' : ' + st );
   end;

  Procedure deviceArrival(m:TYModule);
   var
     serial     : string;
   begin
     serial := m.get_serialNumber();
     writeln('Device arrival : ' + serial);
     m.registerLogCallback(@logfun)
   end;


var
   errmsg:string;

begin
  if (yRegisterHub('usb',  errmsg) <> YAPI_SUCCESS) then
   begin
     WriteLn('RegisterHub error : ' + errmsg);
     halt;
   end;

  yRegisterDeviceArrivalCallback(@deviceArrival);
  
  WriteLn('Hit Ctrl-C to Stop ');

  while(true) do
   begin
      yUpdateDeviceList( errmsg); // traps plug/unplug events
      ySleep(500, errmsg);   // traps others events
   end;

end.