unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, yocto_api, yocto_temperature, yocto_humidity,
  ComCtrls;

type
  TForm1 = class(TForm)
    ComboBox1: TComboBox;
    Label1: TLabel;
    Timer1: TTimer;
    tempLabel: TLabel;
    HumLabel: TLabel;
    StatusBar1: TStatusBar;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }
     Procedure modulesInventory();
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure devicelistchanged(m:tymodule);

 begin
   // something has changed in the devices list
   // lets refresh it, quick and dirty way.
   form1.modulesInventory();
 end;


Procedure TForm1.modulesInventory();

  var
   module         : TYModule;
   name,lname     : string;
   currentModule  : Tymodule;
   index,i        : integer;
 begin

   // memorize the current selection
   currentModule := nil;
   if (combobox1.itemindex>=0) then
       currentModule := TYModule(combobox1.items.objects[combobox1.itemindex]);

   // update the list, brute force
   combobox1.items.clear;
   module := yFirstModule();
   while module<>nil  do
   begin
     name  :=  module.get_serialNumber();
     if copy(name,1,8)='HUMSENS1' then
      begin
       lname :=  module.get_logicalName();
       if (lname<>'') then  name:=name+' ('+lname+')';
       combobox1.items.AddObject(name,module);
      end;
      module := module.nextModule();
   end;

   // restore previous selection
   if (combobox1.items.count=0) then
    begin
      combobox1.enabled:=false;
    end
    else
    begin
     combobox1.enabled:=true;
     index :=0;
     for i:=0 to combobox1.items.count-1 do
       if  (combobox1.items.objects[i]=currentModule) then index:=i;
     combobox1.itemindex:=index;
    end;

  if combobox1.items.count =0 then  StatusBar1.panels[0].text:='No device connected'
  else if combobox1.items.count =1 then  StatusBar1.panels[0].text:='One device connected'
  else StatusBar1.panels[0].text:=intToStr(combobox1.items.count)+' devices connected';


end;


// some hardware init
procedure TForm1.Timer1Timer(Sender: TObject);
  var
   errmsg : string;
   module : TyModule;
   Tfunc  : TyTemperature;
   Hfunc  : TyHumidity;


begin
   if (comboBox1.itemindex>=0) then
    begin
      module:= TyModule(comboBox1.items.objects[comboBox1.itemindex]);
      if module.isOnline() then
       begin
        Tfunc:=yFindTemperature(module.get_SerialNumber()+'.temperature');
        Hfunc:=yFindHumidity(module.get_SerialNumber()+'.humidity');
        tempLabel.caption:= floatToStr(Tfunc.get_currentValue())+'°C';
        humLabel.caption:= floatToStr(Hfunc.get_currentValue())+'%';
       end;
     end
   else
    begin
      tempLabel.caption:= 'N/A';
      humLabel.caption:= 'N/A';
    end;

    yUpdateDeviceList(errmsg); // scan for devices list changes
end;




procedure TForm1.FormCreate(Sender: TObject);
 
begin
   ModulesInventory();
   // we waana know when device list changes
   yRegisterDeviceArrivalCallBack( devicelistchanged);
   yRegisterDeviceRemovalCallBack( devicelistchanged);
   timer1.enabled:=true;

end;


 var
   errmsg: string;

initialization
  if yRegisterHub('usb', errmsg)<>YAPI_SUCCESS then
    begin
      Application.MessageBox(pchar('Yocto-api init failed: '+errmsg), 'Error', MB_OK);
      application.terminate();
    end;
   yDisableExceptions();


end.


