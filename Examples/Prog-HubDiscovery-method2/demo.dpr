program demo;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  yocto_api;

// hub discovery, method 2 :  this example
// will register any and all hubs found.

procedure  arrivalCallback(dev : TYModule);
  var
    isAHub :boolean;
    fctCount,i :integer;
    fctHwdName,deviceid  :string;
  begin
   isAHub := false;
   // iterate on all functions on the module and find the ports
   fctCount :=  dev.functionCount();
   for i:=0 to fctCount-1 do
    begin
     // retreive the hardware name of the ith function
     fctHwdName := dev.functionId(i);
     if (copy(fctHwdName,1,7) = 'hubPort') then
       begin
         // the device contains a  hubPortx function, so it's a hub
         if not(isAHub) then
           begin
            writeln('hub found : '+dev.get_friendlyname());
            isAHub:=true;
           end;
         // The port logical name is always the serial#
         // of the connected device
         deviceid :=  dev.functionName(i);
         Writeln(' '+fctHwdName+' : '+deviceid);
       end;
    end;
  end;

var
  errmsg : string;
  i : integer;

begin
  Writeln('Waiting for hubs to signal themselve...');

  // Connect to any hub found on the network
  if (yRegisterHub('net',errmsg)<>YAPI_SUCCESS) then
    begin
     writeln(errmsg);
     exit;
    end;

  // each time a new device is connected/discovered
  // arrivalCallback will be called.
  yRegisterDeviceArrivalCallback(arrivalCallback);

  // wait for 30 seconds, doing nothing.
  for i:=1 to 30 do
   begin
    yUpdateDeviceList(errmsg);
    ySleep(1000,errmsg);
   end;
end.
