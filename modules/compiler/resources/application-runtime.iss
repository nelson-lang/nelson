// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later

function FolderDigest(Name: String): String;
begin
  Result := Lowercase(GetSHA256OfUnicodeString(Lowercase(RemoveBackslashUnlessRoot(ExpandFileName(Name)))));
end;

function ApplicationMetadata: String;
begin
  Result := ExpandConstant('{app}\.nelson-install\application.ini');
  CheckNoRedirect(Result);
  Result := InstallNativePath(Result);
end;

function ReadInstallMode: String;
var FileName: String;
begin
  FileName := ApplicationMetadata;
  RequireRuntime(FileExists(FileName), 'Application installation metadata is missing. Restore it before removing the application.');
  RequireRuntime((GetIniString('Installation', 'Format', '', FileName) = '1') and
    (GetIniString('Installation', 'Application', '', FileName) = ApplicationIdentity) and
    (GetIniString('Installation', 'Engine', '', FileName) = RuntimeFingerprint) and
    (GetIniString('Installation', 'Directory', '', FileName) = FolderDigest(ExpandConstant('{app}'))),
    'Application installation metadata is incompatible or damaged.');
  Result := GetIniString('Installation', 'Mode', '', FileName);
  RequireRuntime((Result = 'private') or (Result = 'shared') or (Result = 'none'), 'Invalid application runtime ownership.');
  if Result = 'private' then
    RequireRuntime(GetIniString('Installation', 'RuntimeDirectory', '', FileName) = FolderDigest(PrivateRuntimeDirectory),
      'The private runtime location changed. Uninstall the previous application first.');
end;

procedure RequirePrivateRuntime;
var Root, Existing: String;
begin
  Root := PrivateRuntimeDirectory;
  CheckNoRedirect(Root);
  RequireRuntime(not FileExists(InstallNativePath(Root)), 'Private runtime destination is not a directory.');
  if RegQueryStringValue(HKCU64, RuntimeRegistryKey, 'Root', Existing) then
    RequireRuntime(FolderDigest(Existing) <> FolderDigest(Root), 'A registered runtime cannot become application-owned.');
  if RegQueryStringValue(HKLM64, RuntimeRegistryKey, 'Root', Existing) then
    RequireRuntime(FolderDigest(Existing) <> FolderDigest(Root), 'A registered runtime cannot become application-owned.');
end;

function PrivateRuntimeUninstaller: String;
var Root, Ledger: String; Found: TFindRec;
begin
  RequirePrivateRuntime;
  Result := '';
  Root := PrivateRuntimeDirectory;
  if not DirExists(InstallNativePath(Root)) then Exit;
  Ledger := Root + '\runtime-install.ini';
  CheckNoRedirect(Ledger);
  Ledger := InstallNativePath(Ledger);
  RequireRuntime((GetIniString('Identity', 'Format', '', Ledger) = '1') and
    (GetIniString('Identity', 'Engine', '', Ledger) = RuntimeFingerprint) and
    (GetIniString('Identity', 'Architecture', '', Ledger) = RuntimeArchitecture), 'Invalid private runtime identity.');
  RequireRuntime(not DirExists(InstallNativePath(Root + '\.nelson-runtime-update')),
    'Recover the interrupted runtime installation before uninstalling.');
  if FindFirst(InstallNativePath(Root + '\unins*.exe'), Found) then try
    repeat
      RequireRuntime((Result = '') and ((Found.Attributes and $410) = 0), 'Ambiguous private runtime uninstaller.');
      Result := Root + '\' + Found.Name;
    until not FindNext(Found);
  finally FindClose(Found); end;
  RequireRuntime(Result <> '', 'Private runtime uninstaller is missing.');
  CheckNoRedirect(ChangeFileExt(Result, '.dat'));
end;

function ContainsFolder(Parent, Child: String): Boolean;
begin
  Parent := AddBackslash(ExpandFileName(Parent));
  Child := AddBackslash(ExpandFileName(Child));
  Result := CompareText(Copy(Child, 1, Length(Parent)), Parent) = 0;
end;

procedure SelectRuntime(var Mode, Root: String);
var Requested: String;
begin
  Requested := ExpandConstant('{param:NELSONRUNTIME|}');
  StringChangeEx(Requested, '/', '\', True);
  Mode := 'none';
  Root := '';
  if Requested <> '' then begin
    RequireRuntime(RuntimeIncluded, 'runtimeFolder requires RuntimeDelivery installer.');
    RequireRuntime(not PortableInstall, 'runtimeFolder requires a registered, non-portable installation.');
    RequireRuntime(((Copy(Requested, 1, 2) = '\\') or (Copy(Requested, 2, 2) = ':\')) and
      (Pos('"', Requested) = 0), 'runtimeFolder must be absolute.');
    Root := AddBackslash(ExpandFileName(Requested)) + RuntimeFingerprint;
    Mode := 'shared';
    RequireRuntime(not ContainsFolder(ExpandConstant('{app}'), Root) and
      not ContainsFolder(Root, ExpandConstant('{app}')), 'Application and shared runtime directories must not overlap.');
    CheckNoRedirect(Root);
  end else if RuntimeIncluded then begin
    Mode := 'private';
    Root := PrivateRuntimeDirectory;
    RequirePrivateRuntime;
  end;
end;

procedure CheckApplicationLayout(Mode: String);
var Previous: String; Found: TFindRec;
begin
  CheckNoRedirect(ExpandConstant('{app}'));
  if FileExists(ApplicationMetadata) then begin
    Previous := ReadInstallMode;
    RequireRuntime((Mode = Previous) or ((Mode <> 'private') and (Previous <> 'private')),
      'Uninstall the application before changing private runtime ownership.');
  end else begin
    RequireRuntime(not DirExists(InstallNativePath(PrivateRuntimeDirectory)),
      'Uninstall the previous application before adopting a private runtime.');
    if FindFirst(InstallNativePath(ExpandConstant('{app}\unins*.exe')), Found) then begin
      FindClose(Found);
      RaiseException('Uninstall the previous application before changing its installation layout.');
    end;
    RequireRuntime(not DirExists(InstallNativePath(ExpandConstant('{app}\.nelson-install'))) and
      not FileExists(InstallNativePath(ExpandConstant('{app}\.nelson-install'))), 'Reserved application metadata already exists.');
  end;
  if Mode <> 'private' then begin
    CheckNoRedirect(PrivateRuntimeDirectory);
    RequireRuntime(not DirExists(InstallNativePath(PrivateRuntimeDirectory)) and
      not FileExists(InstallNativePath(PrivateRuntimeDirectory)), 'An adjacent runtime would override shared runtime discovery.');
  end;
end;

procedure LogRuntimeChild(FileName: String);
var Lines: TArrayOfString; I: Integer;
begin
  if LoadStringsFromFile(FileName, Lines) then
    for I := 0 to GetArrayLength(Lines) - 1 do Log('Runtime: ' + Lines[I]);
end;

procedure InstallApplicationRuntime(Mode, Root: String);
var Parameters, FileName, LogName: String; ExitCode: Integer; Started: Boolean;
begin
  if Mode = 'none' then Exit;
  ExtractTemporaryFile('nelson-runtime-package.exe');
  FileName := ExpandConstant('{tmp}\nelson-runtime-package.exe');
  LogName := ExpandConstant('{tmp}\application-runtime.log');
  Parameters := '/VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP- /NOCLOSEAPPLICATIONS /NORESTARTAPPLICATIONS';
  if IsAdminInstallMode then Parameters := Parameters + ' /ALLUSERS'
  else Parameters := Parameters + ' /CURRENTUSER';
  if Mode = 'private' then Parameters := Parameters + ' /PORTABLE=1';
  Parameters := Parameters + ' "/DIR=' + Root + '" "/LOG=' + LogName + '"';
  Started := Exec(FileName, Parameters, '', SW_HIDE, ewWaitUntilTerminated, ExitCode);
  LogRuntimeChild(LogName);
  RequireRuntime(Started and (ExitCode = 0), 'Runtime installation failed (code ' + IntToStr(ExitCode) + '). See the installation log.');
end;

procedure WriteApplicationMetadata(Mode, Root: String);
var FileName, Value: String;
begin
  FileName := ExpandConstant('{tmp}\application.ini');
  RequireRuntime(SetIniString('Installation', 'Format', '1', FileName), 'Cannot stage application metadata.');
  RequireRuntime(SetIniString('Installation', 'Application', ApplicationIdentity, FileName), 'Cannot stage application identity.');
  RequireRuntime(SetIniString('Installation', 'Engine', RuntimeFingerprint, FileName), 'Cannot stage application runtime identity.');
  RequireRuntime(SetIniString('Installation', 'Mode', Mode, FileName), 'Cannot stage application runtime ownership.');
  RequireRuntime(SetIniString('Installation', 'Directory', FolderDigest(ExpandConstant('{app}')), FileName), 'Cannot stage application directory.');
  Value := '';
  if Root <> '' then Value := FolderDigest(Root);
  RequireRuntime(SetIniString('Installation', 'RuntimeDirectory', Value, FileName), 'Cannot stage application runtime directory.');
  Value := '';
  if Mode = 'private' then begin
    Value := PrivateRuntimeUninstaller;
    RequireRuntime(Value <> '', 'Private runtime installation is missing.');
    Value := Lowercase(GetSHA256OfFile(InstallNativePath(Value)));
  end;
  RequireRuntime(SetIniString('Installation', 'Uninstaller', Value, FileName), 'Cannot stage runtime uninstaller identity.');
end;

function PrepareToInstall(var NeedsRestart: Boolean): String;
var Mode, Root: String;
begin
  Result := '';
  try
    SelectRuntime(Mode, Root);
    CheckApplicationLayout(Mode);
    InstallApplicationRuntime(Mode, Root);
    WriteApplicationMetadata(Mode, Root);
  except Result := GetExceptionMessage; end;
end;

function CheckedPrivateUninstaller: String;
begin
  Result := '';
  if ReadInstallMode <> 'private' then Exit;
  Result := PrivateRuntimeUninstaller;
  if Result <> '' then
    RequireRuntime(Lowercase(GetSHA256OfFile(InstallNativePath(Result))) =
      GetIniString('Installation', 'Uninstaller', '', ApplicationMetadata), 'The private runtime uninstaller changed.');
end;

function InitializeUninstall: Boolean;
var FileName: String;
begin
  Result := False;
  try
    FileName := CheckedPrivateUninstaller;
    Result := True;
  except Log(GetExceptionMessage); end;
end;

procedure CurUninstallStepChanged(Step: TUninstallStep);
var FileName: String; ExitCode: Integer;
begin
  if Step = usDone then RemoveDir(InstallNativePath(ExpandConstant('{app}')));
  if Step <> usUninstall then Exit;
  // Run only after the user confirms removal, before the application loses its recovery metadata.
  FileName := CheckedPrivateUninstaller;
  if FileName = '' then Exit;
  RequireRuntime(Exec(FileName, '/VERYSILENT /SUPPRESSMSGBOXES /NORESTART', '', SW_HIDE,
    ewWaitUntilTerminated, ExitCode) and (ExitCode = 0), 'Private runtime removal failed. Application removal was cancelled.');
end;
