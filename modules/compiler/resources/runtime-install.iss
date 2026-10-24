// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
// Included in the generated [Code] section; no preprocessor expansion is needed.

var RuntimeLock: THandle;
    RuntimeRecoveryActive: Boolean;

function CreateRuntimeLock(Security: THandle; Owned: Integer; Name: String): THandle;
  external 'CreateMutexW@kernel32.dll stdcall';
function RuntimeLastError: Cardinal;
  external 'GetLastError@kernel32.dll stdcall';
function CloseRuntimeLock(Handle: THandle): Integer;
  external 'CloseHandle@kernel32.dll stdcall';

function AcquireRuntimeLock: Boolean;
var Code: Cardinal;
begin
  Result := True;
  if RuntimeLock <> 0 then Exit;
  RuntimeLock := CreateRuntimeLock(0, 0, RuntimeLockName);
  Code := RuntimeLastError;
  Result := (RuntimeLock <> 0) and (Code <> 183);
  if not Result and (RuntimeLock <> 0) then begin
    CloseRuntimeLock(RuntimeLock);
    RuntimeLock := 0;
  end;
end;

procedure ReleaseRuntimeLock;
begin
  if RuntimeLock <> 0 then begin
    CloseRuntimeLock(RuntimeLock);
    RuntimeLock := 0;
  end;
end;

procedure DeinitializeSetup;
begin
  ReleaseRuntimeLock;
end;

function InitializeUninstall: Boolean;
begin
  Result := AcquireRuntimeLock;
  if not Result then Log('Another operation is using this shared runtime.');
  if Result and DirExists(InstallNativePath(ExpandConstant('{app}\.nelson-runtime-update'))) then begin
    Log('Recover the interrupted runtime installation before uninstalling.');
    Result := False;
  end;
end;

procedure DeinitializeUninstall;
begin
  ReleaseRuntimeLock;
end;

// INSTALLER_PATH_CODE

function InventoryCount(FileName, Section: String; Limit: Integer): Integer;
begin
  Result := GetIniInt(Section, 'Count', -1, -1, Limit + 1, FileName);
  RequireRuntime((Result >= 0) and (Result <= Limit), 'Invalid runtime inventory count.');
end;

function InventoryValue(FileName, Section: String; Index: Integer): String;
begin
  Result := GetIniString(Section, IntToStr(Index), '', FileName);
  RequireRuntime((Result <> '') and (Length(Result) <= 8192), 'Invalid runtime inventory entry.');
end;

procedure CheckDigest(Value: String);
var I: Integer;
begin
  RequireRuntime(Length(Value) = 64, 'Invalid runtime digest.');
  for I := 1 to Length(Value) do
    RequireRuntime(Pos(Value[I], '0123456789abcdef') > 0, 'Invalid runtime digest.');
end;

procedure CheckPlain(Value: String);
var I: Integer;
begin
  for I := 1 to Length(Value) do
    RequireRuntime(Ord(Value[I]) >= 32, 'Control character in runtime inventory.');
end;

procedure CheckPath(Name: String);
var Part, Rest, Device: String; P: Integer;
begin
  RequireRuntime((Name <> '') and (Length(Name) <= 4096), 'Invalid runtime path.');
  CheckPlain(Name);
  RequireRuntime((Pos('\', Name) = 0) and (Pos(':', Name) = 0) and
    (Pos('*', Name) = 0) and (Pos('?', Name) = 0) and (Pos('"', Name) = 0) and
    (Pos('<', Name) = 0) and (Pos('>', Name) = 0) and (Pos('|', Name) = 0), 'Unsafe runtime path.');
  RequireRuntime((CompareText(Name, 'runtime.json') <> 0) and
    (CompareText(Name, 'runtime-install.ini') <> 0) and
    (CompareText(Name, 'modules/modules.m') <> 0) and
    (CompareText(Copy(Name, 1, 5), 'unins') <> 0) and
    (CompareText(Copy(Name, 1, 16), '.nelson-runtime-') <> 0), 'Reserved runtime path.');
  Rest := Name + '/';
  while Rest <> '' do begin
    P := Pos('/', Rest);
    Part := Copy(Rest, 1, P - 1);
    Delete(Rest, 1, P);
    RequireRuntime((Part <> '') and (Part <> '.') and (Part <> '..'), 'Unsafe runtime path component.');
    RequireRuntime((Part[Length(Part)] <> '.') and (Part[Length(Part)] <> ' '), 'Unsafe runtime path suffix.');
    Device := Uppercase(Part);
    P := Pos('.', Device);
    if P > 0 then Device := Copy(Device, 1, P - 1);
    RequireRuntime((Device <> 'CON') and (Device <> 'PRN') and (Device <> 'AUX') and
      (Device <> 'NUL') and not (((Copy(Device, 1, 3) = 'COM') or (Copy(Device, 1, 3) = 'LPT')) and
      (Length(Device) = 4) and (Pos(Device[4], '123456789') > 0)), 'Reserved runtime device name.');
  end;
end;

function RuntimePath(Name: String): String;
begin
  CheckPath(Name);
  StringChangeEx(Name, '/', '\', True);
  Result := AddBackslash(ExpandConstant('{app}')) + Name;
  CheckNoRedirect(Result);
  Result := InstallNativePath(Result);
end;

procedure ReadStartup(FileName: String; Startup: TStringList);
var I, J, Count: Integer; Name: String;
begin
  Count := InventoryCount(FileName, 'Startup', 512);
  RequireRuntime(Count > 0, 'Missing runtime startup ordering.');
  for I := 0 to Count - 1 do begin
    Name := InventoryValue(FileName, 'Startup', I);
    for J := 1 to Length(Name) do
      RequireRuntime(Pos(Name[J], 'abcdefghijklmnopqrstuvwxyz0123456789_') > 0, 'Invalid runtime module name.');
    RequireRuntime(Startup.IndexOf(Name) < 0, 'Duplicate runtime startup module.');
    Startup.Add(Name);
  end;
end;

procedure ReadModules(FileName: String; Startup, Modules: TStringList);
var I, Count: Integer; Name: String;
begin
  Count := InventoryCount(FileName, 'Modules', 512);
  for I := 0 to Count - 1 do begin
    Name := InventoryValue(FileName, 'Modules', I);
    RequireRuntime((Startup.IndexOf(Name) >= 0) and (Name <> 'compiler'), 'Invalid installed runtime module.');
    if Modules.IndexOf(Name) < 0 then Modules.Add(Name);
  end;
  RequireRuntime((Modules.IndexOf('core') >= 0) and (Modules.IndexOf('engine') >= 0) and
    (Modules.IndexOf('interpreter') >= 0), 'Missing required runtime modules.');
end;

function FileDigest(Files: TStringList; Name: String): String;
var Index: Integer; Found: Boolean;
begin
  Result := '';
  Found := Files.Find(Name + '|', Index);
  if Index < Files.Count then
    if CompareText(Copy(Files[Index], 1, Length(Name) + 1), Name + '|') = 0 then
      Result := Copy(Files[Index], Length(Name) + 2, 64);
end;

procedure ReadFiles(FileName: String; Files: TStringList; Installed: Boolean);
var I, P, Count: Integer; Entry, Name, Digest, Target, Existing: String;
begin
  Count := InventoryCount(FileName, 'Files', 100000);
  RequireRuntime(Count > 0, 'Missing runtime files.');
  for I := 0 to Count - 1 do begin
    Entry := InventoryValue(FileName, 'Files', I);
    P := Pos('|', Entry);
    RequireRuntime(P > 1, 'Invalid runtime file record.');
    Name := Copy(Entry, 1, P - 1);
    Digest := Copy(Entry, P + 1, Length(Entry));
    CheckDigest(Digest);
    Target := RuntimePath(Name);
    RequireRuntime(not DirExists(Target), 'Runtime file conflicts with a directory.');
    Existing := FileDigest(Files, Name);
    RequireRuntime((Existing = '') or (Existing = Digest), 'Conflicting runtime file content: ' + Name);
    if Installed or FileExists(Target) then begin
      RequireRuntime(Installed or (Existing <> '') or RuntimeRecoveryActive, 'Unmanaged runtime destination file: ' + Name);
      RequireRuntime(FileExists(Target), 'Installed runtime file is missing: ' + Name);
      RequireRuntime(CompareText(GetSHA256OfFile(Target), Digest) = 0, 'Installed runtime file changed: ' + Name);
    end;
    if Existing = '' then Files.Add(Name + '|' + Digest);
    RequireRuntime(Files.Count <= 100000, 'Runtime union exceeds the file count limit.');
  end;
end;

procedure ReadDirectories(FileName: String; Directories, Files: TStringList);
var I, Count: Integer; Name, Target: String;
begin
  Count := InventoryCount(FileName, 'Directories', 100000);
  for I := 0 to Count - 1 do begin
    Name := InventoryValue(FileName, 'Directories', I);
    Target := RuntimePath(Name);
    RequireRuntime((FileDigest(Files, Name) = '') and not FileExists(Target), 'Runtime directory conflicts with a file.');
    if Directories.IndexOf(Name) < 0 then Directories.Add(Name);
  end;
  RequireRuntime(Files.Count + Directories.Count <= 100000, 'Runtime union exceeds the entry count limit.');
end;

procedure CheckIdentity(PackageFile, InstalledFile: String; Startup: TStringList; MetadataRoot: String);
var I: Integer; Field, Expected, Actual: String; Other: TStringList;
begin
  for I := 0 to 3 do begin
    case I of
      0: Field := 'Format'; 1: Field := 'Architecture';
      2: Field := 'Version'; 3: Field := 'Engine';
    end;
    Expected := GetIniString('Identity', Field, '', PackageFile);
    Actual := GetIniString('Identity', Field, '', InstalledFile);
    RequireRuntime((Expected <> '') and (Expected = Actual), 'Incompatible installed runtime identity.');
  end;
  Other := TStringList.Create;
  try
    ReadStartup(InstalledFile, Other);
    RequireRuntime(Startup.Text = Other.Text, 'Conflicting runtime startup ordering.');
  finally Other.Free; end;
  for I := 0 to 1 do begin
    if I = 0 then begin Field := 'ModulesDigest'; Actual := MetadataRoot + '\modules\modules.m'; end
    else begin Field := 'ManifestDigest'; Actual := MetadataRoot + '\runtime.json'; end;
    Expected := GetIniString('Identity', Field, '', InstalledFile);
    CheckDigest(Expected);
    CheckNoRedirect(Actual);
    Actual := InstallNativePath(Actual);
    RequireRuntime(FileExists(Actual), 'Installed runtime metadata is missing.');
    RequireRuntime(CompareText(GetSHA256OfFile(Actual), Expected) = 0, 'Installed runtime metadata changed.');
  end;
end;

procedure RequireEmptyRuntime;
var Found: TFindRec;
begin
  if FindFirst(InstallNativePath(ExpandConstant('{app}\*')), Found) then try
    repeat
      RequireRuntime((Found.Name = '.') or (Found.Name = '..') or
        (((Found.Attributes and $10) <> 0) and ((Found.Attributes and $400) = 0) and
        (Pos('.nelson-runtime-prepare-', Found.Name) = 1)), 'The runtime destination is not empty or managed.');
    until not FindNext(Found);
  finally FindClose(Found); end;
end;

function RuntimeRegistryHive: Integer;
begin
  if IsAdminInstallMode then Result := HKLM64 else Result := HKCU64;
end;

procedure CheckRegistration;
var Existing: String;
begin
  if not PortableInstall and RegQueryStringValue(RuntimeRegistryHive, RuntimeRegistryKey, 'Root', Existing) then
    RequireRuntime(CompareText(RemoveBackslashUnlessRoot(Existing), ExpandConstant('{app}')) = 0,
      'This shared runtime is already registered at another directory.');
end;

procedure CurUninstallStepChanged(Step: TUninstallStep);
var Existing: String;
begin
  // Preparation creates the root before the installer can record it as a new directory.
  if Step = usDone then RemoveDir(InstallNativePath(ExpandConstant('{app}')));
  if (Step = usPostUninstall) and RegQueryStringValue(RuntimeRegistryHive, RuntimeRegistryKey, 'Root', Existing) then
    if CompareText(RemoveBackslashUnlessRoot(Existing), ExpandConstant('{app}')) = 0 then begin
      RegDeleteValue(RuntimeRegistryHive, RuntimeRegistryKey, 'Root');
      RegDeleteKeyIfEmpty(RuntimeRegistryHive, RuntimeRegistryKey);
    end;
end;

function JsonText(Value: String): String;
begin
  CheckPlain(Value);
  StringChangeEx(Value, '\', '\\', True);
  StringChangeEx(Value, '"', '\"', True);
  Result := '"' + Value + '"';
end;

procedure WriteSection(FileName, Section: String; Values: TStringList);
var I: Integer;
begin
  RequireRuntime(SetIniString(Section, 'Count', IntToStr(Values.Count), FileName), 'Cannot save runtime inventory.');
  for I := 0 to Values.Count - 1 do
    RequireRuntime(SetIniString(Section, IntToStr(I), Values[I], FileName), 'Cannot save runtime inventory entry.');
end;

procedure WriteRuntimeMetadata(PackageFile, InstalledFile: String; Startup, Modules, Files, Directories: TStringList);
var I, P: Integer; Ledger, OrderFile, ManifestFile, OrderText, Suffix, Field, Value: String;
  Order, Json: TStringList;
begin
  Ledger := ExpandConstant('{tmp}\runtime-install.ini');
  OrderFile := ExpandConstant('{tmp}\modules.m');
  ManifestFile := ExpandConstant('{tmp}\runtime.json');
  RequireRuntime(FileCopy(PackageFile, Ledger, False), 'Cannot stage runtime inventory.');
  Order := TStringList.Create;
  Json := TStringList.Create;
  try
    OrderText := 'modules_list = {' + #13#10;
    for I := 0 to Startup.Count - 1 do
      if Modules.IndexOf(Startup[I]) >= 0 then begin
        Order.Add(Startup[I]);
        OrderText := OrderText + '{''' + Startup[I] + ''', true};' + #13#10;
      end;
    OrderText := OrderText + '};' + #13#10;
    RequireRuntime(SaveStringToFile(OrderFile, UTF8Encode(OrderText), False), 'Cannot stage runtime startup ordering.');
    Files.Sort;
    Directories.Sort;
    WriteSection(Ledger, 'Modules', Order);
    WriteSection(Ledger, 'Files', Files);
    WriteSection(Ledger, 'Directories', Directories);
    Json.Add('{"format":1,"kind":"nelson-shared-runtime","mode":"installed",');
    Json.Add('"architecture":' + JsonText(GetIniString('Identity', 'Architecture', '', Ledger)) + ',');
    Json.Add('"runtimeVersion":' + JsonText(GetIniString('Identity', 'Version', '', Ledger)) + ',');
    Json.Add('"engineFingerprint":' + JsonText(GetIniString('Identity', 'Engine', '', Ledger)) + ',');
    for I := 0 to 1 do begin
      if I = 0 then Field := 'GUI' else Field := 'Dynamic';
      Value := GetIniString('Identity', Field, '0', PackageFile);
      if FileExists(InstalledFile) and (GetIniString('Identity', Field, '0', InstalledFile) = '1') then Value := '1';
      RequireRuntime((Value = '0') or (Value = '1'), 'Invalid runtime startup flag.');
      RequireRuntime(SetIniString('Identity', Field, Value, Ledger), 'Cannot save runtime startup flag.');
      if I = 0 then Field := 'gui' else Field := 'dynamicStartup';
      if Value = '1' then Value := 'true' else Value := 'false';
      Json.Add(JsonText(Field) + ':' + Value + ',');
    end;
    Value := '[';
    for I := 0 to Order.Count - 1 do begin
      if I > 0 then Value := Value + ',';
      Value := Value + JsonText(Order[I]);
    end;
    Value := Value + ']';
    Json.Add('"modules":' + Value + ',"moduleOrder":' + Value + ',"files":[');
    for I := 0 to Files.Count - 1 do begin
      Suffix := ',';
      if I = Files.Count - 1 then Suffix := '';
      P := Pos('|', Files[I]);
      Json.Add('{"relative":' + JsonText(Copy(Files[I], 1, P - 1)) + ',"sha256":' +
        JsonText(Copy(Files[I], P + 1, 64)) + '}' + Suffix);
    end;
    Json.Add('],"directories":[');
    for I := 0 to Directories.Count - 1 do begin
      Suffix := ',';
      if I = Directories.Count - 1 then Suffix := '';
      Json.Add(JsonText(Directories[I]) + Suffix);
    end;
    Value := Lowercase(GetSHA256OfFile(OrderFile));
    Json.Add('],"generated":{"relative":"modules/modules.m","sha256":' + JsonText(Value) + '}}');
    RequireRuntime(SetIniString('Identity', 'ModulesDigest', Value, Ledger), 'Cannot save runtime metadata digest.');
    RequireRuntime(SaveStringToFile(ManifestFile, UTF8Encode(Json.Text), False), 'Cannot stage runtime manifest.');
    RequireRuntime(SetIniString('Identity', 'ManifestDigest', Lowercase(GetSHA256OfFile(ManifestFile)), Ledger),
      'Cannot save runtime metadata digest.');
  finally Json.Free; Order.Free; end;
end;

// RUNTIME_RECOVERY_CODE

function PrepareToInstall(var NeedsRestart: Boolean): String;
var PackageFile, InstalledFile: String;
  Startup, Modules, Files, Directories: TStringList;
begin
  Result := '';
  Startup := TStringList.Create;
  Modules := TStringList.Create;
  Files := TStringList.Create;
  Directories := TStringList.Create;
  Files.Sorted := True;
  try
    try
      RequireRuntime(AcquireRuntimeLock, 'Another operation is using this shared runtime.');
      CheckRegistration;
      ExtractTemporaryFile('runtime-package.ini');
      PackageFile := ExpandConstant('{tmp}\runtime-package.ini');
      RecoverRuntimeInstall(PackageFile);
      InstalledFile := ExpandConstant('{app}\runtime-install.ini');
      CheckNoRedirect(InstalledFile);
      InstalledFile := InstallNativePath(InstalledFile);
      RequireRuntime(GetIniString('Identity', 'Format', '', PackageFile) = '1', 'Invalid runtime inventory version.');
      ReadStartup(PackageFile, Startup);
      if FileExists(InstalledFile) then begin
        CheckIdentity(PackageFile, InstalledFile, Startup, ExpandConstant('{app}'));
        ReadModules(InstalledFile, Startup, Modules);
        ReadFiles(InstalledFile, Files, True);
        ReadDirectories(InstalledFile, Directories, Files);
      end else if not RuntimeRecoveryActive then RequireEmptyRuntime;
      ReadModules(PackageFile, Startup, Modules);
      ReadFiles(PackageFile, Files, False);
      ReadDirectories(PackageFile, Directories, Files);
      WriteRuntimeMetadata(PackageFile, InstalledFile, Startup, Modules, Files, Directories);
      PrepareRuntimeJournal(PackageFile);
    except Result := GetExceptionMessage; end;
  finally Directories.Free; Files.Free; Modules.Free; Startup.Free; end;
end;
