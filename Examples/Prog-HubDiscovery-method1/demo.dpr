program demo;
{$APPTYPE CONSOLE}
uses
  SysUtils, classes,
  yocto_api;

var
  list   : Tstringlist;

// This example will found all hubs on the local network,
// connect to them one by one, retreive information, and
// then disconnect.

procedure  HubDiscovered(serial, url: string);
  var
    i :integer;
    hub :TYmodule;
    msg,fctHwdName,deviceid :string;
    fctCount :integeR;
  begin
   // The call-back can be called several times for the same hub
   // (the discovery technique is based on a periodic broadcast)
   // So we use a dictionnary to avoid duplicates
   if  list.indexof(serial)>=0 then exit;

   Writeln('hub found: '+serial+' ('+url+')');

   // connect to the hub
   YRegisterHub(url,msg);

   //  find the hub module
   hub := YfindModule(serial);

   // iterate on all functions on the module and find the ports
   fctCount :=  hub.functionCount();
   for i:=0 to fctCount-1 do
    begin
     // retreive the hardware name of the ith function
     fctHwdName := hub.functionId(i);
     if (copy(fctHwdName,1,7) = 'hubPort') then
       begin
         // The port logical name is always the serial#
         // of the connected device
         deviceid :=  hub.functionName(i);
         Writeln(' '+fctHwdName+' : '+deviceid);
       end;
    end;
   // add the hub to the dictionnary so we won't have to
   // process is again.
   list.add(serial);

   // disconnect from the hub
   YUnRegisterHub(url);
  end;

var
  i:integer;
  errmsg : string;

begin
  // create a dictionnary
  list := Tstringlist.create();
  list.sorted:=true;

  Writeln('Waiting for hubs to signal themselve...');

  // register the callback: HubDiscovered will be
  // invoked each time a hub signals its presence
  yRegisterHubDiscoveryCallback(HubDiscovered);

  // wait for 30 seconds, doing nothing.
  for i:=1 to 30 do
   begin
    yUpdateDeviceList(errmsg);
    ySleep(1000,errmsg);
   end;
end.
