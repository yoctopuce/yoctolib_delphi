program blink_demo;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  yocto_api,
  yocto_colorled;

var
 errmsg : string;
 led:TYColorled;

begin
  // Insert user code here
  if (yregisterhub('usb',errmsg)<>YAPI_SUCCESS)   then
      begin
       writeln(errmsg);
       halt;
      end;

  led := YFirstColorLed();
  if led = nil then
    begin
       writeln('no color ,led found, check USB cable');
       halt;
      end;

  led.resetBlinkSeq();                       // cleans the sequence
                                             // preprogram the sequence
  led.addRgbMoveToBlinkSeq($00FF00,500);     // move to green in 500 ms
  led.addRgbMoveToBlinkSeq($000000,   0);    // switch to black instantaneously
  led.addRgbMoveToBlinkSeq($000000,  250);   // stays black for 250ms
  led.addRgbMoveToBlinkSeq($0000FF,    0);   // switch to blue instantaneously
  led.addRgbMoveToBlinkSeq($0000FF,  100);   // stays blue for 100ms
  led.addRgbMoveToBlinkSeq($000000,   0);    // switch to black instantaneously
  led.addRgbMoveToBlinkSeq($000000,  250);   // stays black for 250ms
  led.addRgbMoveToBlinkSeq($FF0000,    0);   // switch to red instantaneously
  led.addRgbMoveToBlinkSeq($FF0000,  100);   // stays red for 100ms
  led.addRgbMoveToBlinkSeq($000000,    0);   // switch to black instantaneously
  led.addRgbMoveToBlinkSeq($000000, 1000);   // stays black for 1s
  led.startBlinkSeq();                       // starts sequence 

  writeln('done');




end. 