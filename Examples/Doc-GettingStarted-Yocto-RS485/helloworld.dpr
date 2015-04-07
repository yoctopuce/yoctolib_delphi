program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  Windows,
  yocto_api,
  yocto_serialport;

var
 errmsg,line  : string;
 serialPort : TYserialport;
 slave,reg: integer;
 res : TLongIntArray;
 cmd :string;
 val : integer;
begin

  // Setup the API to use local USB devices. You can
  // use an IP address instead of 'usb' if the device
  // is connected to a network.
  if (YRegisterHub('usb', errmsg) <> YAPI_SUCCESS)  then
    begin
      writeln('RegisterHub error: ' + errmsg);
      halt;
    end;

  if (paramcount>1) then
       serialPort := YFindSerialPort(paramstr(1))
    else
     begin
       serialPort := YFirstSerialPort();
       if  (serialPort=nil) then
         begin
           writeln('No module connected (check cable)');
           halt;
         end;
     end;


  writeln('Please enter the MODBUS slave address (1...255)');
  repeat
   ReadLn(slave);
  until (slave>0) and (slave<256);

  writeln('Please select a Coil No (>=1), Input Bit No (>=10001+),');
  writeln('Register No (>=30001) or Input Register No (>=40001)');
  writeln('No: ');
  repeat
  ReadLn(reg);
  until (reg >=1) and  (reg<50000) and ((reg mod 10000)<> 0);

  while (true)  do
   begin
    if (reg>=40001) then res := serialPort.modbusReadInputRegisters(slave, reg-40001, 1)
    else if (reg>=30001) then res := serialPort.modbusReadRegisters(slave, reg-30001, 1)
    else if (reg>=10001) then res := serialPort.modbusReadInputBits(slave, reg-10001, 1)
    else res := serialPort.modbusReadBits(slave, reg-1, 1);
    val := res[0];
    writeln('Current value: '+inttostr(val));
    write('Press ENTER to read again, Q to quit');
    if((reg mod 30000) < 10000) then write (' or enter a new value');
    write(': ');
    readLn(cmd);
    if (cmd ='q') or  (cmd ='Q') then halt;
    if  (cmd<>'') and ((reg mod 30000) < 10000) then
     begin
         val := strtoint(cmd);
         if(reg >= 30001) then serialPort.modbusWriteRegister(slave, reg-30001, val)
                          else    serialPort.modbusWriteBit(slave, reg-1, val);
     end;
   end;

end.
