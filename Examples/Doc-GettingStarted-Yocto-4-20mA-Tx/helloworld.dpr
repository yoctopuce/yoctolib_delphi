program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  yocto_api,
  yocto_currentLoopOutput;

Procedure Usage();
 var
   exe,errmsg : string;
 begin
   exe := ExtractFileName(paramstr(0));
   Writeln('Bad command line arguments');
   writeln('usage:');
   writeln(' '+exe+' serial valuw');
   writeln(' '+exe+' logicalName value');
   writeln(' '+exe+' any value');
   writeln(' ');
   writeln('Example:');
   writeln(' '+exe+' TX420MA1-123456 12');
   writeln(' '+exe+' any 20');
   ySleep(2500,errmsg);
   halt;
 end;

Procedure error(err:string);
  var errmsg:string;
 begin
    Writeln(err);
    ySleep(2500,errmsg);
    halt;
 end;

var
  loop      : TYCurrentLoopOutput;
  value     : double;
  errmsg    : string;
  pwr       : integer;
begin
  // Setup the API to use local USB devices
  if yRegisterHub('usb', errmsg)<>YAPI_SUCCESS then
     error('RegisterHub error: '+errmsg);

  // check parameters
  if (paramcount<>2) then usage();

  // retreive the loop controler
  if (paramstr(1)='any') then
     loop :=yFirstCurrentLoopOutput()
    else
     loop :=yFindCurrentLoopOutput(paramstr(1)+'.currentLoopOutput');

  // is the module connected
  if not(loop.isOnline()) then
     error('device is not connected, check parameters / cable');

  // set current loop
  value :=StrToFloat(paramstr(2)) ;
  loop.set_current(value);

  // check loop power
  pwr:=loop.get_loopPower();
  if (pwr=Y_LOOPPOWER_NOPWR)then
      error('Current loop not powered');

  if (pwr=Y_LOOPPOWER_LOWPWR)then
      error('Insufficient voltage on current loop');

  writeln('current loop set to ' + FloatToStrF(value,ffFixed,2,2) + ' mA');
  ySleep(2500,errmsg);
end.
