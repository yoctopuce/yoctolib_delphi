{*********************************************************************
 *
 *  $Id: helloworld.lpr 59602 2024-03-04 09:18:09Z seb $
 *
 *  An example that shows how to use a  Yocto-Buzzer
 *
 *  You can find more information on our web site:
 *   Yocto-Buzzer documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-buzzer/doc.html
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
  yocto_buzzer,
  yocto_anbutton,
  yocto_led;

Procedure  Usage();
  var
   exe : string;

  begin
    exe:= ExtractFileName(paramstr(0));
    WriteLn(exe+' <serial_number> frequency');
    WriteLn(exe+' <logical_name> frequency');
    WriteLn(exe+' any frequency');
    WriteLn('');
    WriteLn('Example:');
    WriteLn(exe+' any 1000');
    sleep(3000);
    halt;
  End;

var
  m               : TYmodule;
  buz             : TYBuzzer;
  led,led1,led2   : TYLed;
  button1,button2 : TYAnButton;
  b1,b2           : boolean;
  frequency       : integer;
  i               : integer;
  serial          : string;
  errmsg          : string;

begin

  if (paramcount<1) then usage();

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
      buz := yFirstbuzzer();
      if buz=nil then
         begin
           writeln('No module connected (check USB cable)');
           sleep(3000);
           halt;
         end
      end
  else // or use the one specified the command line
    buz:= YFindBuzzer(paramstr(1)+'.buzzer');

  // make sure it connected
  if not buz.isOnline() then
    begin
       writeln('No module connected (check USB cable)');
       sleep(3000);
       halt;
    end;

  m:= buz.get_module();
  serial := m.get_serialNumber();
  led1:= YFindLed(serial+'.led1');
  led2:= YFindLed(serial+'.led2');
  button1 := YFindAnButton(serial+'.anButton1');
  button2 := YFindAnButton(serial+'.anButton2');

  writeln('press a test button or hit Ctrl-C');
  while true do
    begin
     b1 := button1.get_isPressed() = Y_ISPRESSED_TRUE;
     b2 := button2.get_isPressed() = Y_ISPRESSED_TRUE;
     if b1 or b2 then
       begin
        if (b1) then begin led:=led1;frequency:=1500;end
                else begin led:=led2;frequency:=750;end;
        led.set_power(Y_POWER_ON);
        led.set_luminosity(100);
        led.set_blinking(Y_BLINKING_PANIC);
        for  i:=0 to 4  do  // this can be done using sequence as well
          begin
            buz.set_frequency(frequency);
            buz.freqMove(2 * frequency, 250);
            YSleep(250,errmsg);
          end;
        buz.set_frequency(0);
        led.set_power(Y_POWER_OFF);
     end;
   end;
  yFreeAPI();
end.