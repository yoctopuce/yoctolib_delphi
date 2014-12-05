program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  Windows,
  yocto_api,
  Classes;

  Procedure upgradeSerialList(allserials :TStringList);
    var
      serial,errmsg  : string;
      newfirm : string;
      product, current : string;
      i,status,newstatus : Integer;
      module : TYModule;
      update : TYFirmwareUpdate;
    begin
      // Now display these animals
      for i := 0 to allserials.Count-1 do
      begin
        serial := allserials[i];
        module := yFindModule(serial);
        product := module.get_productName();
        current := module.get_firmwareRelease();

        // check if a new firmare is available on yoctopuce.com
        newfirm := module.checkFirmware('www.yoctopuce.com', true);
        if newfirm = '' then
          writeln(product + ' ' + serial + '(rev=' + current + ') is up to date')
        else
          begin
          writeln(product + ' ' + serial + '(rev=' + current + ') need be updated with firmare : ');
          writeln('    ' + newfirm);
          // execute the firmware upgrade
          update := module.updateFirmware(newfirm);
          status := update.startUpdate();
          Repeat
            newstatus := update.get_progress();
            if newstatus <> status then
              writeln(inttostr(status) + '% ' + update.get_progressMessage());
            ySleep(500, errmsg);
            status := newstatus;
          Until (status >= 100) or (status < 0);
          if (status < 0) then
            begin
              writeln('    ' + inttostr(status) + ' Firmware Update failed: ' + update.get_progressMessage());
              halt;
            end
          else
            begin
              if module.isOnline() then
                writeln(inttostr(status) + '% Firmware Updated Successfully!')
              else
                begin
                  writeln(inttostr(status) + ' Firmware Update failed: module ' + serial + 'is not online');
                  halt;
                end;
          end;
      end;
   end;
End;


var
  errmsg  : string;
  serial  : string;
  product : string;
  hubs    : TStringList;
  shield  : TStringList;
  devices : TStringList;
  module  : TYmodule;

begin
  hubs := TStringList.Create();
  shield := TStringList.Create();
  devices := TStringList.Create();

  // Setup the API to use local USB devices. You can
  // use an IP address instead of 'usb' if the device
  // is connected to a network.
  if (YRegisterHub('usb', errmsg) <> YAPI_SUCCESS)  then
    begin
      writeln('RegisterHub error: ' + errmsg);
      halt;
    end;

  module := yFirstModule();
  while module<>nil do
    begin
      serial := module.get_serialNumber();
      product := module.get_productName();
      if (product = 'YoctoHub-Shield') then
        shield.Add(serial)
      else
        if (copy(product,1,9)='YoctoHub-') then
          hubs.Add(serial)
        else
        devices.Add(serial);
      module := module.nextModule();
    end;
    // fist upgrades all Hubs...
    upgradeSerialList(hubs);
    // ... then all shield..
    upgradeSerialList(shield);
    // ... and finaly all devices
    upgradeSerialList(devices);
    writeln('All devices are now up to date');
    yFreeAPI();
end.
