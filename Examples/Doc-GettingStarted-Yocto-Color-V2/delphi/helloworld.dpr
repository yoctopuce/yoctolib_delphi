{*********************************************************************
 *
 *  $Id: helloworld.dpr 58172 2023-11-30 17:10:23Z martinm $
 *
 *  An example that shows how to use a  Yocto-Color-V2
 *
 *  You can find more information on our web site:
 *   Yocto-Color-V2 documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-color-v2/doc.html
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
  yocto_colorledcluster;

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

procedure assignColor(ledCluster:TYColorLedCluster; nb_leds:integer; color:integer);
  begin
    if (ledCluster.isOnline()) then
     begin
      // immediate transition for fist half of leds
      ledCluster.set_rgbColor(0, nb_leds div 2, color);
      // immediate transition for second half of leds
      ledCluster.rgb_move(nb_leds div 2, nb_leds div 2, color, 2000);
     end
    else Writeln('Module not connected (check identification and USB cable)');
  end;

var
  c          : char;
  ledCluster : TYColorLedCluster;
  errmsg     : string;
  serial     : string;
  nb_leds    : integer;

begin

  if (paramcount<1) then usage();

  // Setup the API to use local USB devices
  if yRegisterHub('usb', errmsg)<>YAPI_SUCCESS then
  begin
    Write('RegisterHub error: '+errmsg);
    sleep(3000);
    exit;
  end;

  // first one of the two RBG leds
  if paramstr(1)='any' then
    begin
      ledCluster := yFirstColorLedCluster();
      if ledCluster=nil then
         begin
           writeln('No module connected (check USB cable)');
           sleep(3000);
           halt;
         end
      end
   else
  ledCluster:= YFindColorLedCluster(paramstr(1)+'.colorledCluster');

  // make sure it is online
  if not(ledCluster.isOnline()) then
      begin
        writeln('No module connected (check USB cable)');
        sleep(3000);
        halt;
      end;
  nb_leds := 2;
  ledCluster.set_activeLedCount(nb_leds);
  ledCluster.set_ledType(Y_LEDTYPE_RGB);

  Writeln('r: set to red');
  Writeln('g: set to green');
  Writeln('b: set to blue');
  Writeln('x: exit');
  repeat
    read(c);
    case c  of
      'r' : assignColor(ledCluster,nb_leds,$FF0000);
      'g' : assignColor(ledCluster,nb_leds,$00FF00);
      'b' : assignColor(ledCluster,nb_leds,$0000FF);
    end;
  until c='x';
  yFreeAPI();

end.