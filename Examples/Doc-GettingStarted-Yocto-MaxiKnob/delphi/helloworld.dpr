{*********************************************************************
 *
 *  $Id: helloworld.dpr 58172 2023-11-30 17:10:23Z martinm $
 *
 *  An example that shows how to use a  Yocto-MaxiKnob
 *
 *  You can find more information on our web site:
 *   Yocto-MaxiKnob documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-maxiknob/doc.html
 *   Delphi API Reference:
 *      https://www.yoctopuce.com/EN/doc/reference/yoctolib-delphi-EN.html
 *
 *********************************************************************}

{*********************************************************************
 *
 *  $Id: helloworld.dpr 58172 2023-11-30 17:10:23Z martinm $
 *
 *  An example that shows how to use a  Yocto-MaxiKnob
 *
 *  You can find more information on our web site:
 *   Yocto-MaxiBuzzer documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-maxiknob/doc.html
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
  yocto_colorledcluster,
  yocto_quadraturedecoder;

Procedure  Usage();
  var
   exe : string;

  begin
    exe:= ExtractFileName(paramstr(0));
    WriteLn(exe+' <serial_number>');
    WriteLn(exe+' <logical_name>');
    WriteLn(exe+' any ');
    WriteLn('');
    WriteLn('Example:');
    WriteLn(exe+' any');
    sleep(3000);
    halt;
  End;

 function notefreq( note:integer) :integer;
    begin
      notefreq := round(220.0 * Exp(note * ln(2) / 12));
    end ;

var
  m               : TYmodule;
  buz             : TYBuzzer;
  leds            : TYColorLedCluster;
  button          : TYAnButton;
  qd              : TYQuadratureDEcoder;
  lastPos,p         : integer;
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
  leds:= YFindColorLedCluster(serial+'.colorLedCluster');
  button := YFindAnButton(serial+'.anButton1');
  qd := YFindQuadratureDecoder(serial+'.quadratureDecoder1');

  writeln('press button #1, or turn the encoder #1 or hit Ctrl-C');
  lastPos := round(qd.get_currentValue());
  buz.set_volume(75);
  while (button.isOnline())do
    begin
      if ((button.get_isPressed()=Y_ISPRESSED_TRUE) and (lastPos <> 0))  then
      begin
          lastPos := 0;
          qd.set_currentValue(0);
          buz.playNotes('''E32 C8');
          leds.set_rgbColor(0, 1, $000000);
      end
        else
       begin
          p := round(qd.get_currentValue());
          if (lastPos <> p) then
          begin
            lastPos := p;
            buz.pulse(notefreq(p), 500);
            leds.set_hslColor(0, 1, $00FF7f or (p mod 255) shl 16);
         end

      end
  end;

  yFreeAPI();
end.