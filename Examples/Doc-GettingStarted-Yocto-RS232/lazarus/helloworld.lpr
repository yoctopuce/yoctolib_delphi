{*********************************************************************
 *
 *  $Id: helloworld.lpr 47192 2021-11-08 18:02:19Z seb $
 *
 *  An example that show how to use a  Yocto-RS232
 *
 *  You can find more information on our web site:
 *   Yocto-RS232 documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-rs232/doc.html
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
