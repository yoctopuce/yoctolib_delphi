{*********************************************************************
 *
 *  $Id: helloworld.dpr 47192 2021-11-08 18:02:19Z seb $
 *
 *  An example that show how to use a  Yocto-Knob
 *
 *  You can find more information on our web site:
 *   Yocto-Knob documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-knob/doc.html
 *   Delphi API Reference:
 *      https://www.yoctopuce.com/EN/doc/reference/yoctolib-delphi-EN.html
 *
 *********************************************************************}

program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,  {$IFNDEF UNIX}
  windows,
  {$ENDIF UNIX} 
  yocto_api,
  yocto_anbutton;

Procedure  Usage();
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

var
  input,input1,input5  : TYAnButton;
  m                    : TYmodule;
  errmsg,serial        : string;
  done                 : boolean;
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
      // try to find the first available anButton
      input := yFirstAnButton();
      if input=nil then
        begin
          writeln('No module connected (check USB cable)');
          sleep(3000);
          halt;
        end
    end
  else  // or use the module specified on the command line
    input:= YFindAnButton(paramstr(1)+'.anButton');

  // make sure it is online
  if not(input.isOnline()) then
    begin
      writeln('No module connected (check USB cable)');
      sleep(3000);
      halt;
    end;

  // lets find the matching module, to find out what serial it have
  m:=input.get_module();
  serial := m.get_serialNumber();

  // now we can reteive the 1srt and 5th button on the same module
  input1 := yFindAnButton(serial+'.anButton1');
  input5 := yFindAnButton(serial+'.anButton5');

  // lets poll
  repeat
    if (not(input1.isOnline())) then
      begin
        Writeln('Module not connected (check identification and USB cable)');
        done := true;
      end
    else
      begin
        if input1.get_isPressed() = Y_ISPRESSED_TRUE
          then Write('Button1: pressed    ')
          else Write('Button1: not pressed');
        Writeln(' - analog value: '+FloatToStr(input1.get_calibratedValue()));
        if input5.get_isPressed() = Y_ISPRESSED_TRUE
          then Write('Button5: pressed    ')
          else Write('Button5: not pressed');
        Writeln(' - analog value: '+FloatToStr(input5.get_calibratedValue()));
        Writeln('(press both buttons simultaneously to exit)');
        done := (input1.get_isPressed() = Y_ISPRESSED_TRUE) and
                (input5.get_isPressed() = Y_ISPRESSED_TRUE);
        Sleep(1000);
      end
  until done;
  yFreeAPI();

end.