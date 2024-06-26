{*********************************************************************
 *
 *  $Id: helloworld.dpr 58172 2023-11-30 17:10:23Z martinm $
 *
 *  An example that shows how to use a  Yocto-Serial
 *
 *  You can find more information on our web site:
 *   Yocto-Serial documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-serial/doc.html
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
  yocto_serialport;

var
 errmsg,target,channel:string;
 serialport:TYserialport;
 line:string;

begin
  if (YRegisterHub('usb', errmsg) <> YAPI_SUCCESS)  then
    begin
      writeln('RegisterHub error: ' + errmsg);
      halt;
    end;

  if (paramcount>1) then
    begin
      target := paramstr(1);
      serialport := YFindSerialPort(target + '.serialPort');
    end
  else
    begin
      serialport := YFirstSerialPort();
      if (serialport = nil) then
         begin
           writeln('No module connected (check USB cable)');
           sleep(3000);
           halt;
         end;
    end;
    
  writeln('****************************');
  writeln('* make sure voltage levels *'); 
  writeln('* are properly configured  *');
  writeln('****************************');  
    
  serialport.set_serialMode('9600,8N1');
  serialPort.set_protocol('Line');
  serialPort.reset();
  repeat
    ySleep(500, errmsg);
    repeat
      line := serialPort.readLine();
      if (line <> '') then
        begin
          writeln('Received: ' + line);
        end;
    until (line = '');
    writeln('Type line to send, or Ctrl-C to exit: ');
    readLn(line);
    serialPort.writeLine(line);
  until (line = '');
  yFreeAPI();
end.
