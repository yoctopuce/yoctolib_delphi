{*********************************************************************
 *
 *  $Id: helloworld.lpr 59602 2024-03-04 09:18:09Z seb $
 *
 *  An example that shows how to use a  Yocto-LatchedRelay
 *
 *  You can find more information on our web site:
 *   Yocto-LatchedRelay documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-latchedrelay/doc.html
 *   Delphi API Reference:
 *      https://www.yoctopuce.com/EN/doc/reference/yoctolib-delphi-EN.html
 *
 *********************************************************************}

program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  {$IFNDEF UNIX}
  windows,
  {$ENDIF UNIX} 
  yocto_api,
  yocto_relay;

Procedure  Usage();
  var
   exe : string;

  begin
    exe:= ExtractFileName(paramstr(0));
    WriteLn(exe+' <serial_number>  A|B');
    WriteLn(exe+' <logical_name> A|B');
    WriteLn(exe+' any A|B');
    WriteLn('');
    WriteLn('Example:');
    WriteLn(exe+' any B');
    sleep(3000);
    halt;
  End;

procedure setRelayState(relay:TYRelay; state:boolean);
  begin
    if (relay.isOnline()) then
     begin
       if state then relay.set_state(Y_STATE_B)
                else relay.set_state(Y_STATE_A);
     end
    else Writeln('Module not connected (check identification and USB cable)');
  end;

var

  relay     : TYRelay;
  errmsg    : string;

begin

  if (paramcount<2) then usage();

  // Setup the API to use local USB devices
  if yRegisterHub('usb', errmsg)<>YAPI_SUCCESS then
  begin
    Write('RegisterHub error: '+errmsg);
    sleep(3000);
    exit;
  end;

  if paramstr(1)='any' then
    begin
      // try to first the first relay available
      relay := yFirstRelay();
      if relay=nil then
         begin
           writeln('No module connected (check USB cable)');
           sleep(3000);
           halt;
         end
       end
  else // or use the one specified the command line
  relay:= YFindRelay(paramstr(1)+'.relay1');

  // make sure it connected
  if not relay.isOnline() then
    begin
       writeln('No module connected (check USB cable)');
       sleep(3000);
       halt;
    end;

   // lets drive the relay
  if paramstr(2)='B' then   setRelayState(relay,true)
                      else   setRelayState(relay,false);
  yFreeAPI();

end.