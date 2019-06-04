program demo;
{$APPTYPE CONSOLE}
uses
  SysUtils,Classes,yocto_api,yocto_anButton,yocto_hubport;

type
  TYoctoShield = class(Tobject)
  private
    _serial : string;
    _subDevices : tstringlist;
  public
    constructor Create(serial:string);
    function getSerial():string;
    function addSubdevice(serial:string):boolean;
    procedure removeSubDevice(serial:string);
    procedure describe();
  end;

  TYoctoShieldArray = array of TYoctoShield;

  TRootDevice = class(Tobject)
  private
    _serial : string;
    _url : string;
    _shields : tlist;
    _subDevices : tstringlist;
  public
    constructor Create(serial:string; url:string);
    function getSerial():string;
    procedure addSubDevice(serial:string);
    procedure removeSubDevice(serial:string);
    procedure describe();
  end;

  TRootDeviceArray = array of TRootDevice;

  constructor TYoctoShield.Create(serial:string);
    begin
      self._serial := serial;
      self._subDevices := tstringlist.create();
    end;

  function TYoctoShield.getSerial():string;
    begin
      result := self._serial;
    end;

  function TYoctoShield.addSubdevice(serial:string):boolean;
    var
      i  : integer;
      p : TYHubPort;
      i_val : string;
    begin
      for i := 1 to 4 do
        begin
          i_val := inttostr(i);
          p := yFindHubPort(self._serial + '.hubPort' + i_val);
          if (p.get_logicalName() = serial) then
            begin
              self._subDevices.Add(serial);
              result := True;
              exit;
            end;
        end;
      result := false;
    end;

  procedure TYoctoShield.removeSubDevice(serial:string);
    var
      i, len :integer;
    begin
      len := self._subDevices.Count;
      for i := len - 1  downto 0  do
        begin
          if self._subDevices[i] = serial then
            self._subDevices.Delete(i);
        end;
    end;
  procedure TYoctoShield.describe();
    var
      i : integer;
    begin
      writeln(self._serial);
      for i := 0 to self._subDevices.Count - 1 do
        writeln('    ' + self._subDevices[i]);
    end;

  constructor TRootDevice.Create(serial:string; url:string);
    begin
      self._serial := serial;
      self._url := url;
      self._shields := tlist.create();
      self._subDevices := tstringlist.create();
    end;

  function TRootDevice.getSerial():string;
    begin
      result := self._serial;
    end;

  procedure TRootDevice.addSubdevice(serial:string);
    var
      i  : integer;
      base_serial : string;
    begin
      base_serial := copy(serial, 1 , 7);
      if base_serial = 'YHUBSHL' then
        begin
          self._shields.Add(TYoctoShield.create(serial));
        end
      else
        begin
          // Device to plug look if the device is plugged on a shield
          for i := 0 to self._shields.Count - 1 do
            if (TYoctoShield(self._shields[i]).addSubdevice(serial)) then
              exit;

          self._subDevices.Add(serial);
        end;
    end;

  procedure TRootDevice.removeSubDevice(serial:string);
    var
      i, len :integer;
    begin
      len := self._subDevices.Count;
      for i := len - 1  downto 0  do
        begin
          if self._subDevices[i] = serial then
            self._subDevices.Delete(i);
        end;
      len := self._shields.Count;
      for i := len - 1  downto 0  do
        begin
          TYoctoShield(self._shields[i]).removeSubDevice(serial);
          if TYoctoShield(self._shields[i]).getSerial() = serial then
            begin
              self._shields.Delete(i);
            end
          else
            begin
              TYoctoShield(self._shields[i]).removeSubDevice(serial)
            end;
        end;
    end;
  procedure TRootDevice.describe();
    var
      i : integer;
    begin
      writeln(self._serial+ ' (' + self._url + ')');
      for i := 0 to self._subDevices.Count - 1 do
        writeln('    ' + self._subDevices[i]);
      for i := 0 to self._shields.Count - 1 do
        TYoctoShield(self._shields[i]).describe();
    end;

var
  __rootDevices :Tlist;

  function getYoctoHub(serial:string):TRootDevice;
    var
      i : integer;
    begin
      for i :=0 to __rootDevices.Count - 1 do
        if (TRootDevice(__rootDevices[i]).getSerial() = serial) then
          begin
            result := TRootDevice(__rootDevices[i]);
            exit;
          end;
      result := nil;
    end;


  function addRootDevice(serial:string; url:string):TRootDevice;
    var
      i : integer;
      rootDevice : TRootDevice;
    begin
      for i :=0 to __rootDevices.Count - 1 do
          if (TRootDevice(__rootDevices[i]).getSerial() = serial) then
            begin
              result := TRootDevice(__rootDevices[i]);
              exit;
            end;
      rootDevice := TRootDevice.Create(serial, url);
      __rootDevices.Add(rootDevice);
      result := rootDevice;
    end;

  procedure showNetwork();
    var
      i : integer;
    begin
      Writeln('**** device inventory *****');
      for i :=0 to __rootDevices.Count - 1 do
          TRootDevice(__rootDevices[i]).describe();
    end;

  Procedure deviceArrival(m:TYModule);
    var
      serial,url : string;
      parentHub : string;
      hub : TRootDevice;

    begin
      serial := m.get_serialNumber();
      parentHub := m.get_parentHub();
      if (parentHub = '') then
        begin
          // root device
          url := m.get_url();
          addRootDevice(serial, url);
        end
      else
        begin
          hub := getYoctoHub(parentHub);
          if hub <> nil then
              hub.addSubDevice(serial);
        end;
     end;

  Procedure deviceRemoval( m: TYmodule);
    var
      serial :string;
      i, len :integer;
    begin
      serial := m.get_serialNumber();
      len := __rootDevices.Count;
      for i := len - 1  downto 0  do
        begin
          TRootDevice(__rootDevices[i]).removeSubDevice(serial);
          if TRootDevice(__rootDevices[i]).getSerial() = serial then
            begin
              __rootDevices.delete(i);
            end;
        end;
    end;

var
  errmsg:string;
begin
  __rootDevices := tlist.create();
  if (yRegisterHub('usb',  errmsg) <> YAPI_SUCCESS) then
   begin
     WriteLn('RegisterHub error : ' + errmsg);
     halt;
   end;

  if (yRegisterHub('net',  errmsg) <> YAPI_SUCCESS) then
   begin
     WriteLn('RegisterHub error : ' + errmsg);
     halt;
   end;

  yRegisterDeviceArrivalCallback(@deviceArrival);
  yRegisterDeviceRemovalCallback(@deviceRemoval);

  WriteLn('Hit Ctrl-C to Stop ');

  while(true) do
   begin
      yUpdateDeviceList( errmsg); // traps plug/unplug events
      ySleep(500, errmsg);   // traps others events
      showNetwork();
   end;

end.