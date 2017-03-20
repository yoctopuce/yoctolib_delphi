program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  yocto_api,
  yocto_colorled;

Procedure  Usage();
  var
   exe : string;

  begin
    exe:= ExtractFileName(paramstr(0));
    WriteLn(exe+' <serial_number>');
    WriteLn(exe+' <logical_name>');
    WriteLn(exe+' any');
    halt;
  End;

procedure setcolor(led1,led2:TYColorLed; color:integer);
  begin
    if (led1.isOnline()) then
     begin
      led1.set_rgbColor(color);  // immediate switch
      led2.rgbMove(color,1000);  // smooth transition
     end
    else Writeln('Module not connected (check identification and USB cable)');
  end;

var
  c         : char;
  led1,led2 : TYColorLed;
  m         : TYmodule;
  errmsg    : string;
  serial    : string;

begin

  if (paramcount<1) then usage();

  // Setup the API to use local USB devices
  if yRegisterHub('usb', errmsg)<>YAPI_SUCCESS then
  begin
    Write('RegisterHub error: '+errmsg);
    exit;
  end;

  // first one of the two RBG leds
  if paramstr(1)='any' then
    begin
      led1 := yFirstColorLed();
      if led1=nil then
         begin
           writeln('No module connected (check USB cable)');
           halt;
         end
      end
   else
  led1:= YFindColorLed(paramstr(1)+'.colorled1');

  // make sure it is online
  if not(led1.isOnline()) then
      begin
        writeln('No module connected (check USB cable)');
        halt;
      end;

  // lets find the device serial to find the second led
  m := led1.Get_module();
  serial := m.get_serialNumber();
  led1 := yFindColorLed(serial+'.colorLed1');
  led2 := yFindColorLed(serial+'.colorLed2');

  Writeln('r: set to red');
  Writeln('g: set to green');
  Writeln('b: set to blue');
  Writeln('x: exit');
  repeat
    read(c);
    case c  of
      'r' : setcolor(led1,led2,$FF0000);
      'g' : setcolor(led1,led2,$00FF00);
      'b' : setcolor(led1,led2,$0000FF);
    end;
  until c='x';
  yFreeAPI();

end.