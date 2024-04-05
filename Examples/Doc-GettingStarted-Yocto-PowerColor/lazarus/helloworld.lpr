{*********************************************************************
 *
 *  $Id: helloworld.lpr 59602 2024-03-04 09:18:09Z seb $
 *
 *  An example that shows how to use a  Yocto-PowerColor
 *
 *  You can find more information on our web site:
 *   Yocto-PowerColor documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-powercolor/doc.html
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
   sleep(3000);
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
    sleep(3000);
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
  yFreeAPI();
end.