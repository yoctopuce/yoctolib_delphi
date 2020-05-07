{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  An example that show how to use a  yocto-MaxiBuzzer
 *
 *  You can find more information on our web site:
 *   yocto-MaxiBuzzer documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-maxibuzzer/doc.html
 *   Delphi API Reference:
 *      https://www.yoctopuce.com/EN/doc/reference/yoctolib-delphi-EN.html
 *
 *********************************************************************}

program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  yocto_api,
  yocto_buzzer,
  yocto_anbutton,
  yocto_colorled;

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
  halt;
  End;

var
  m               : TYmodule;
  buz             : TYBuzzer;
  led             : TYColorLed;
  button1,button2 : TYAnButton;
  b1,b2           : boolean;
  frequency       : integer;
  color           : integer;
  volume          : integer;
  i               : integer;
  serial          : string;
  errmsg          : string;

begin

  if (paramcount<1) then usage();

  // Setup the API to use local USB devices
  if yRegisterHub('usb', errmsg)<>YAPI_SUCCESS then
  begin
    Write('RegisterHub error: '+errmsg);
    exit;
  end;

  if paramstr(1)='any' then
    begin
      // try to first the first relay available
      buz := yFirstbuzzer();
      if buz=nil then
         begin
           writeln('No module connected (check USB cable)');
           halt;
         end
      end
  else // or use the one specified the command line
    buz:= YFindBuzzer(paramstr(1)+'.buzzer');

  // make sure it connected
  if not buz.isOnline() then
    begin
       writeln('No module connected (check USB cable)');
       halt;
    end;

  m:= buz.get_module();
  serial := m.get_serialNumber();
  led:= YFindColorLed(serial+'.colorLed');
  button1 := YFindAnButton(serial+'.anButton1');
  button2 := YFindAnButton(serial+'.anButton2');

  writeln('press a test button or hit Ctrl-C');
  while true do
    begin
     b1 := button1.get_isPressed() = Y_ISPRESSED_TRUE;
     b2 := button2.get_isPressed() = Y_ISPRESSED_TRUE;
     if b1 or b2 then
       begin
        if (b1) then begin volume:=60;color:=$ff0000;frequency:=1500;end
                else begin volume:=30;color:=$00ff00;;frequency:=750;end;
        led.resetBlinkSeq();
        led.addRgbMoveToBlinkSeq(color, 100);
        led.addRgbMoveToBlinkSeq(0, 100);
        led.startBlinkSeq();
        buz.set_volume(volume);
        for  i:=0 to 4  do  // this can be done using sequence as well
          begin
            buz.set_frequency(frequency);
            buz.freqMove(2 * frequency, 250);
            YSleep(250,errmsg);
          end;
        buz.set_frequency(0);
        led.stopBlinkSeq();
        led.set_rgbColor(0);
     end;
   end;
  yFreeAPI();
end.