{*********************************************************************
 *
 *  $Id: inventory.dpr 47192 2021-11-08 18:02:19Z seb $
 *
 *  Doc-Inventory example
 *
 *  You can find more information on our web site:
 *   Delphi API Reference:
 *      https://www.yoctopuce.com/EN/doc/reference/yoctolib-delphi-EN.html
 *
 *********************************************************************}

program inventory;
{$APPTYPE CONSOLE}
uses
  SysUtils,Classes,
  yocto_api;


function LoadCertFromFile(const Host: string): string;
var
  CertFile: TFileStream;
  CertData: TStringStream;
  Path: string;
begin
  Path := Host + '.crt';
  Result := '';
  if FileExists(Path) then
  begin
    CertFile := TFileStream.Create(Path, fmOpenRead);
    try
      CertData := TStringStream.Create;
      try
        CertData.CopyFrom(CertFile, CertFile.Size);
        Result := CertData.DataString;
      finally
        CertData.Free;
      end;
    finally
      CertFile.Free;
    end;
  end;
end;

procedure SaveCertToFile(const Host, Cert: string);
var
  CertFile: TFileStream;
  Path: string;
  Bytes: TBytes;
  I: integer;
begin
  Path := Host + '.crt';
  SetLength(Bytes, Length(Cert));
  for I := 0 to Length(Cert) - 1 do
    Bytes[I] := Byte(Cert[I + 1]);
  CertFile := TFileStream.Create(Path, fmCreate);
  try
    CertFile.WriteBuffer(Bytes[0], Length(Bytes));
  finally
    CertFile.Free;
  end;
end;


var
  module : TYModule;
  errmsg : string;
  username: string;
  password : string;
  host:string;
  url:string;
  trusted_cert:string;
  res:integer;
  line:string;
  c:string;


begin
  username := 'admin';
  password := '1234';
  host := 'localhost';
  url := 'secure://' + username + ':' + password + '@' + host;
  // load known TLS certificate into the API
  trusted_cert := LoadCertFromFile(host);
  if trusted_cert <> '' then
    begin
      errmsg := yAddTrustedCertificates(trusted_cert);
      if errmsg <> '' then
        begin
          Write(errmsg);
          exit;
        end;
    end;
    // test connection with VirtualHub
    res := yTestHub(url, 1000, errmsg);
    if res = YAPI_SSL_UNK_CERT then
      begin
        // Remote TLS certificate is unknown ask user what to do
        Writeln('Remote SSL/TLS certificate is unknown');
        Writeln('You can...');
        Writeln(' -(A)dd certificate to the API');
        Writeln(' -(I)gnore this error and continue');
        Writeln(' -(E)xit');
        Write('Your choice: ');
        Readln(Line);
        c := LowerCase(Line[1]);
        if c = 'a' then
          begin
            // Download remote certificate and save it locally
            trusted_cert := yDownloadHostCertificate(Url, 5000);
            if Pos('error', LowerCase(trusted_cert)) = 1 then
              begin
                Writeln(trusted_cert);
                Exit;
              end;
            SaveCertToFile(host, trusted_cert);
            errmsg := yAddTrustedCertificates(trusted_cert);
            if errmsg <> '' then
              begin
                Write(errmsg);
                exit;
              end;
          end
        else if c = 'i' then
          begin
            ySetNetworkSecurityOptions(Y_NO_HOSTNAME_CHECK or Y_NO_TRUSTED_CA_CHECK or Y_NO_EXPIRATION_CHECK);
          end
        else if c = 'e' then
          begin
            Exit;
          end;
      end
    else if Res <> YAPI_SUCCESS then
      begin
        Writeln('YAPI::TestHub failed: ' + ErrMsg);
        Exit;
      end;



  if yRegisterHub(url, errmsg)<>YAPI_SUCCESS then
  begin
    Write('RegisterHub error: '+errmsg);
    exit;
  end;

  Writeln('Device list');

  module := yFirstModule();
  while module<>nil  do
   begin
     Writeln( module.get_serialNumber()+' ('+module.get_productName()+')');
     module := module.nextModule();
   end;
  yFreeAPI();

end.