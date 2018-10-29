{*********************************************************************
 *
 *  $Id: helloworld.dpr 32621 2018-10-10 13:10:25Z seb $
 *
 *  An example that show how to use a  Yocto-Serial
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
