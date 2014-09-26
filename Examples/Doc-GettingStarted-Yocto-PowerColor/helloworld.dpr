program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  Windows,
  yocto_api,
  yocto_colorled;

Procedure Usage();
 var
   exe : string;
 begin
   exe := ExtractFileName(paramstr(0));
   Writeln('Bad command line arguments');
   writeln('usage:');
   writeln(' '+exe+' serial color');
   writeln(' '+exe+' logicalName color');
   writeln(' '+exe+' any color');
   writeln(' ');
   writeln('Example:');
   writeln(' '+exe+' YRGBHI0-123456 FF0000');
   writeln(' '+exe+' any            0000FF');
   sleep(2500);
   halt;
 end;

var
  led1      : TYColorLed;
  color     : integer;
  errmsg    : string;
begin
  // Setup the API to use local USB devices
  if yRegisterHub('usb', errmsg)<>YAPI_SUCCESS then
  begin
    Write('RegisterHub error: '+errmsg);
    exit;
  end;

  // check parameters
  if (paramcount<>2) then usage();

  // retreive the led
  if (paramstr(1)='any') then
     led1 :=yFirstColorLed()
    else
     led1 :=yFindColorLed(paramstr(1)+'.colorLed1');

  // is the module connected
  if not(led1.isOnline()) then
   begin
    Writeln('device is not connected, check parameters / cable');
    ySleep(2500,errmsg);
    halt;
   end;

  // change color
  color :=StrToInt('$' + paramstr(2)) ;
  led1.rgbMove(color,1000); // smooth change.
end.