program helloworld;
{$APPTYPE CONSOLE}
uses
  windows,
  SysUtils,
  yocto_api,
  yocto_messagebox;

Procedure Usage();
  var
   exe : string;
  begin
    exe:= ExtractFileName(paramstr(0));
    WriteLn(exe+' <serial_number>');
    WriteLn(exe+' <logical_name>');
    WriteLn(exe+' any');
    sleep(3000);
    halt;
  End;

Procedure smsCallback(mbox: TYMessageBox; sms: TYSms);
  begin
    writeln('- dated ' + sms.get_timestamp());
    writeln('  from ' + sms.get_sender());
    writeln('  "' + sms.get_textData() + '"');
    sms.deleteFromSIM();
  End;

var
  errmsg, number : String;
  mbox           : TYMessageBox;
  messages       : TYSmsArray;
  sms            : TYSms;
  i              : integer;

begin

  if (paramcount<1) then usage();

  // Setup the API to use local USB devices
  if yRegisterHub('usb', errmsg)<>YAPI_SUCCESS then
  begin
    Write('RegisterHub error: '+errmsg);
    exit;
  end;

  // find a hub with messaging capability
  if paramstr(1)='any' then
    begin
      mbox := yFirstMessageBox();
      if mbox = nil then
         begin
           writeln('No module with SMS features (check USB cable)');
           halt;
         end
      end
    else
      mbox := YFindMessageBox(paramstr(1)+'.messageBox');

  writeln;
  writeln('Using '+mbox.get_FriendlyName());
  writeln;

  // make sure it is online
  if not(mbox.isOnline()) then
      begin
        writeln('GSM hub not found (check USB cable)');
        halt;
      end;

  // list messages found on the device
  Writeln('Messages found on the SIM card:');
  messages := mbox.get_messages();
  for i := 0 to length(messages)-1 do
    begin
      sms := messages[i];
      writeln('- dated ' + sms.get_timestamp());
      writeln('  from ' + sms.get_sender());
      writeln('  "' + sms.get_textData() + '"');
    end;

  // register a callback to receive any new message
  mbox.registerSmsCallback(smsCallback);

  // offer to send a new message
  writeln('To test sending SMS, provide message recipient (+xxxxxxx).');
  writeln('To skip sending, leave empty and press Enter.');
  ReadLn(number);
  if number <> '' then
    begin
      // if that call fails, make sure that your SIM operator
      // allows you to send SMS given your current contract
      mbox.sendTextMessage(number, 'Hello from YoctoHub-GSM !');
    end;

  writeln('Waiting to receive SMS, press Ctrl-C to quit');
  while true do
    ySleep(3000, errmsg);

end.

