// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
// Included after the runtime inventory helpers, before PrepareToInstall.

function NativeRecoveryMove(Source, Destination: String; Flags: Cardinal): Boolean;
  external 'MoveFileExW@kernel32.dll stdcall';

function RecoveryMove(Source, Destination: String; Flags: Cardinal): Boolean;
var Code: Cardinal; Attempt: Integer;
begin
  for Attempt := 0 to 40 do begin
    Result := NativeRecoveryMove(Source, Destination, Flags);
    if Result then Exit;
    Code := RuntimeLastError;
    if (Attempt = 40) or ((Code <> 5) and (Code <> 32) and (Code <> 33)) then Break;
    if Attempt = 0 then Log('Retrying busy runtime recovery move (Windows ' + IntToStr(Code) + ').');
    Sleep(50);
  end;
  Log('Runtime recovery move failed (Windows ' + IntToStr(Code) + '): ' + Source + ' -> ' + Destination);
end;

function RecoveryRoot: String;
begin
  Result := ExpandConstant('{app}\.nelson-runtime-update');
  CheckNoRedirect(Result);
  Result := InstallNativePath(Result);
end;

function RecoverySettings: String;
begin
  if IsAdminInstallMode then Result := 'machine' else Result := 'user';
  if PortableInstall then Result := Result + ',portable' else Result := Result + ',registered';
end;

function RecoveryDestinationDigest: String;
begin
  // INI files use the system code page; hash explicit UTF-8 destination bytes.
  Result := GetSHA256OfString(UTF8Encode(Lowercase(ExpandConstant('{app}'))));
end;

function RecoveryMetadata(Index: Integer): String;
begin
  case Index of
    0: Result := 'runtime-install.ini';
    1: Result := 'runtime.json';
    2: Result := 'modules/modules.m';
  end;
end;

function RecoveryPath(Root, Name: String): String;
begin
  StringChangeEx(Name, '/', '\', True);
  Result := AddBackslash(Root) + Name;
  CheckNoRedirect(Result);
  Result := InstallNativePath(Result);
end;

procedure CheckRecoveryName(Name: String);
var I: Integer;
begin
  for I := 0 to 2 do if Name = RecoveryMetadata(I) then Exit;
  RequireRuntime((Length(Name) = 12) and (Copy(Name, 1, 5) = 'unins') and
    (Pos(Name[6], '0123456789') > 0) and (Pos(Name[7], '0123456789') > 0) and
    (Pos(Name[8], '0123456789') > 0) and
    ((Copy(Name, 9, 4) = '.exe') or (Copy(Name, 9, 4) = '.dat') or (Copy(Name, 9, 4) = '.msg')),
    'Invalid runtime recovery filename.');
end;

function RecoveryHash(FileName: String): String;
var Found: TFindRec;
begin
  CheckNoRedirect(FileName);
  RequireRuntime(FindFirst(FileName, Found), 'Missing runtime recovery file.');
  try
    RequireRuntime(((Found.Attributes and $10) = 0) and (Found.SizeHigh = 0) and
      (Found.SizeLow <= 67108864), 'Invalid or oversized runtime recovery file.');
  finally FindClose(Found); end;
  Result := Lowercase(GetSHA256OfFile(FileName));
end;

function OptionalRecoveryHash(FileName: String): String;
begin
  CheckNoRedirect(FileName);
  RequireRuntime(not DirExists(FileName), 'Runtime metadata conflicts with a directory.');
  Result := '-';
  if FileExists(FileName) then Result := RecoveryHash(FileName);
end;

procedure RecoverySave(Journal, Section, Name, Value: String);
begin
  RequireRuntime(SetIniString(Section, Name, Value, Journal), 'Cannot save runtime recovery journal.');
end;

function RecoveryDirectory(Prefix: String): String;
var I: Integer;
begin
  for I := 0 to 4095 do begin
    Result := RecoveryPath(ExpandConstant('{app}'), Prefix + IntToStr(I));
    if not FileExists(Result) and not DirExists(Result) then Exit;
  end;
  RaiseException('Too many retained runtime recovery directories.');
end;

function RecoverySequence(Name, Prefix, Suffix: String): Boolean;
var Number: String; Value: Integer;
begin
  Result := False;
  if (Copy(Name, 1, Length(Prefix)) <> Prefix) or
    (Copy(Name, Length(Name) - Length(Suffix) + 1, Length(Suffix)) <> Suffix) then Exit;
  Number := Copy(Name, Length(Prefix) + 1, Length(Name) - Length(Prefix) - Length(Suffix));
  Value := StrToIntDef(Number, -1);
  Result := (Value >= 0) and (Value < 4096) and (IntToStr(Value) = Number);
end;

function KnownRecoveryFile(Name: String; Names: TStringList): Boolean;
var I: Integer; Tail: String;
begin
  Result := (Name = 'journal.ini') or (Name = 'package.ini') or (Name = 'complete') or
    RecoverySequence(Name, 'restore-', '.tmp') or RecoverySequence(Name, 'commit-', '.tmp');
  if Copy(Name, 1, 9) = 'previous/' then
    Result := Names.IndexOf(Copy(Name, 10, Length(Name))) >= 0;
  if Copy(Name, 1, 5) = 'next/' then
    for I := 0 to 2 do if Name = 'next/' + RecoveryMetadata(I) then Result := True;
  if Copy(Name, 1, 10) = 'preserved/' then begin
    Tail := Copy(Name, 11, Length(Name));
    if (Length(Tail) < 14) or (Tail[13] <> '.') then Exit;
    CheckRecoveryName(Copy(Tail, 1, 12));
    Result := (Copy(Tail, 1, 5) = 'unins') and
      RecoverySequence(Tail, Copy(Tail, 1, 13), '');
  end;
end;

procedure CheckRecoveryTree(Root, Relative: String; Names: TStringList; var Count: Integer);
var Found: TFindRec; Name, Target: String;
begin
  Target := RecoveryPath(Root, Relative);
  if not DirExists(Target) then Exit;
  if FindFirst(AddBackslash(Target) + '*', Found) then try
    repeat
      if (Found.Name <> '.') and (Found.Name <> '..') then begin
        Count := Count + 1;
        RequireRuntime(Count <= 16384, 'Too many runtime recovery entries.');
        Name := Relative + Found.Name;
        Target := RecoveryPath(Root, Name);
        if (Found.Attributes and $10) <> 0 then begin
          RequireRuntime((Name = 'previous') or (Name = 'previous/modules') or
            (Name = 'next') or (Name = 'next/modules') or (Name = 'preserved'),
            'Unknown runtime recovery content.');
          CheckRecoveryTree(Root, Name + '/', Names, Count);
        end else begin
          RequireRuntime(KnownRecoveryFile(Name, Names), 'Unknown runtime recovery content.');
          RequireRuntime((Found.SizeHigh = 0) and (Found.SizeLow <= 67108864),
            'Oversized runtime recovery file.');
        end;
      end;
    until not FindNext(Found);
  finally FindClose(Found); end;
end;

procedure CheckRecoveryJournal(Root: String; Names: TStringList);
var Journal, Name, Digest: String; I, Count: Integer;
begin
  Journal := RecoveryPath(Root, 'journal.ini');
  RecoveryHash(Journal);
  RequireRuntime(GetIniString('State', 'Format', '', Journal) = '1', 'Invalid runtime recovery journal.');
  RequireRuntime(GetIniString('State', 'RootSHA256', '', Journal) = RecoveryDestinationDigest,
    'Runtime recovery belongs to another destination.');
  RequireRuntime(GetIniString('State', 'Settings', '', Journal) = RecoverySettings,
    'Runtime recovery cannot use different installation settings.');
  Digest := GetIniString('State', 'Package', '', Journal);
  CheckDigest(Digest);
  RequireRuntime(RecoveryHash(RecoveryPath(Root, 'package.ini')) = Digest, 'Changed runtime recovery package.');
  Count := InventoryCount(Journal, 'Names', 64);
  RequireRuntime(Count >= 6, 'Incomplete runtime recovery inventory.');
  for I := 0 to Count - 1 do begin
    Name := InventoryValue(Journal, 'Names', I);
    CheckRecoveryName(Name);
    RequireRuntime(Names.IndexOf(Name) < 0, 'Duplicate runtime recovery filename.');
    Names.Add(Name);
    Digest := GetIniString('Previous', Name, '', Journal);
    if Digest <> '-' then CheckDigest(Digest);
    RequireRuntime(OptionalRecoveryHash(RecoveryPath(Root + '\previous', Name)) = Digest,
      'Changed or missing runtime recovery backup.');
  end;
  for I := 0 to 2 do begin
    Name := RecoveryMetadata(I);
    RequireRuntime(Names.IndexOf(Name) >= 0, 'Missing runtime metadata recovery record.');
    Digest := GetIniString('Next', Name, '', Journal);
    CheckDigest(Digest);
    RequireRuntime(RecoveryHash(RecoveryPath(Root + '\next', Name)) = Digest, 'Changed runtime recovery metadata.');
  end;
  Count := 0;
  CheckRecoveryTree(Root, '', Names, Count);
end;

procedure RecoveryCopy(Source, Destination: String);
begin
  CheckNoRedirect(Destination);
  RequireRuntime(ForceDirectories(ExtractFileDir(Destination)), 'Cannot create runtime recovery directory.');
  RequireRuntime(FileCopy(Source, Destination, True), 'Cannot copy runtime recovery state.');
  RequireRuntime(RecoveryHash(Source) = RecoveryHash(Destination), 'Runtime recovery copy changed.');
end;

procedure PrepareRuntimeJournal(PackageFile: String);
var Root, Journal, Name, Source, Digest: String; Names: TStringList; I: Integer; Found: TFindRec;
begin
  if RuntimeRecoveryActive then begin
    Journal := RecoveryPath(RecoveryRoot, 'journal.ini');
    for I := 0 to 2 do begin
      Name := RecoveryMetadata(I);
      if I = 2 then Source := ExpandConstant('{tmp}\modules.m')
      else Source := ExpandConstant('{tmp}\') + Name;
      RequireRuntime(RecoveryHash(Source) = GetIniString('Next', Name, '', Journal),
        'Runtime recovery no longer matches this installer.');
    end;
    Exit;
  end;
  Root := RecoveryDirectory('.nelson-runtime-prepare-');
  RequireRuntime(ForceDirectories(ExpandConstant('{app}')) and CreateDir(Root), 'Cannot prepare runtime recovery.');
  Journal := Root + '\journal.ini';
  Names := TStringList.Create;
  try
    for I := 0 to 2 do Names.Add(RecoveryMetadata(I));
    Names.Add('unins000.exe'); Names.Add('unins000.dat'); Names.Add('unins000.msg');
    if FindFirst(InstallNativePath(ExpandConstant('{app}\unins*')), Found) then try
      repeat
        CheckRecoveryName(Found.Name);
        if Names.IndexOf(Found.Name) < 0 then Names.Add(Found.Name);
        RequireRuntime(Names.Count <= 64, 'Too many runtime uninstall files.');
      until not FindNext(Found);
    finally FindClose(Found); end;
    RecoverySave(Journal, 'State', 'Format', '1');
    RecoverySave(Journal, 'State', 'RootSHA256', RecoveryDestinationDigest);
    RecoverySave(Journal, 'State', 'Settings', RecoverySettings);
    RecoverySave(Journal, 'State', 'Package', RecoveryHash(PackageFile));
    RecoveryCopy(PackageFile, Root + '\package.ini');
    WriteSection(Journal, 'Names', Names);
    for I := 0 to Names.Count - 1 do begin
      Name := Names[I];
      Source := RecoveryPath(ExpandConstant('{app}'), Name);
      Digest := OptionalRecoveryHash(Source);
      RecoverySave(Journal, 'Previous', Name, Digest);
      if Digest <> '-' then RecoveryCopy(Source, RecoveryPath(Root + '\previous', Name));
    end;
    for I := 0 to 2 do begin
      Name := RecoveryMetadata(I);
      if I = 2 then Source := ExpandConstant('{tmp}\modules.m')
      else Source := ExpandConstant('{tmp}\') + Name;
      RecoveryCopy(Source, RecoveryPath(Root + '\next', Name));
      RecoverySave(Journal, 'Next', Name, RecoveryHash(Source));
    end;
    RequireRuntime(RecoveryMove(Root, RecoveryRoot, 8), 'Cannot activate runtime recovery journal.');
    RuntimeRecoveryActive := True;
    Log('Runtime recovery journal activated.');
  finally Names.Free; end;
end;

procedure CheckRecoveryTargets(Root: String; Names: TStringList);
var Journal, Name, Actual, Previous, Next: String; I: Integer;
begin
  Journal := Root + '\journal.ini';
  for I := 0 to Names.Count - 1 do begin
    Name := Names[I];
    Actual := OptionalRecoveryHash(RecoveryPath(ExpandConstant('{app}'), Name));
    Previous := GetIniString('Previous', Name, '', Journal);
    Next := GetIniString('Next', Name, '', Journal);
    // Uninstall logs are generated by the installer; changed copies are retained, never discarded.
    RequireRuntime((Copy(Name, 1, 5) = 'unins') or (Actual = '-') or
      (Actual = Previous) or (Actual = Next), 'Runtime metadata changed after interruption: ' + Name);
  end;
end;

procedure PreserveUninstallAttempt(Root, Name, Target: String);
var Directory, Saved: String; I: Integer;
begin
  Directory := RecoveryPath(Root, 'preserved');
  RequireRuntime(ForceDirectories(Directory), 'Cannot preserve interrupted uninstall state.');
  for I := 0 to 4095 do begin
    Saved := RecoveryPath(Directory, Name + '.' + IntToStr(I));
    if not FileExists(Saved) and not DirExists(Saved) then begin
      RequireRuntime(RecoveryMove(Target, Saved, 8), 'Cannot preserve interrupted uninstall state.');
      Exit;
    end;
  end;
  RaiseException('Too many interrupted uninstall state copies.');
end;

procedure RestoreRuntimeMetadata(Root: String; Names: TStringList);
var Journal, Name, Target, Previous, Actual, Temporary: String; I, Attempt: Integer;
begin
  Journal := Root + '\journal.ini';
  for I := 0 to Names.Count - 1 do begin
    Name := Names[I];
    Target := RecoveryPath(ExpandConstant('{app}'), Name);
    Previous := GetIniString('Previous', Name, '', Journal);
    Actual := OptionalRecoveryHash(Target);
    if Actual <> Previous then begin
      if (Copy(Name, 1, 5) = 'unins') and (Actual <> '-') then
        PreserveUninstallAttempt(Root, Name, Target);
      if Previous = '-' then begin
        if FileExists(Target) then RequireRuntime(DeleteFile(Target), 'Cannot remove interrupted runtime metadata.');
      end else begin
        Attempt := 0;
        repeat
          Temporary := RecoveryPath(Root, 'restore-' + IntToStr(Attempt) + '.tmp');
          Attempt := Attempt + 1;
          RequireRuntime(Attempt <= 4096, 'Too many interrupted metadata restorations.');
        until not FileExists(Temporary) and not DirExists(Temporary);
        RequireRuntime(FileCopy(RecoveryPath(Root + '\previous', Name), Temporary, True), 'Cannot stage runtime metadata restoration.');
        RequireRuntime(RecoveryHash(Temporary) = Previous, 'Changed runtime restoration bytes.');
        RequireRuntime(ForceDirectories(ExtractFileDir(Target)), 'Cannot restore runtime metadata directory.');
        RequireRuntime(RecoveryMove(Temporary, Target, 9), 'Cannot restore runtime metadata.');
      end;
    end;
  end;
end;

procedure CleanRecoveryDirectory(Directory: String; Names: TStringList);
var I: Integer; Target: String;
begin
  for I := 0 to Names.Count - 1 do begin
    Target := RecoveryPath(Directory, Names[I]);
    if FileExists(Target) then RequireRuntime(DeleteFile(Target), 'Cannot clean runtime recovery file.');
  end;
  Target := RecoveryPath(Directory, 'modules');
  if DirExists(Target) then RequireRuntime(RemoveDir(Target), 'Unknown runtime recovery content.');
  if DirExists(Directory) then RequireRuntime(RemoveDir(Directory), 'Unknown runtime recovery content.');
end;

procedure FinishRuntimeJournal(Root: String; Names: TStringList);
var Saved: String; Found: TFindRec; Keep: Boolean;
begin
  Saved := RecoveryDirectory('.nelson-runtime-recovered-');
  RequireRuntime(RecoveryMove(Root, Saved, 8), 'Cannot retire completed runtime recovery journal.');
  Root := Saved;
  // Retain installer-generated uninstall state from interrupted attempts for inspection.
  Keep := DirExists(RecoveryPath(Root, 'preserved'));
  if FindFirst(Root + '\restore-*.tmp', Found) then begin Keep := True; FindClose(Found); end;
  if FindFirst(Root + '\commit-*.tmp', Found) then begin Keep := True; FindClose(Found); end;
  if Keep then begin
    Log('Interrupted uninstall state retained in ' + Saved);
    Exit;
  end;
  CleanRecoveryDirectory(Root + '\previous', Names);
  CleanRecoveryDirectory(Root + '\next', Names);
  RequireRuntime(DeleteFile(Root + '\package.ini'), 'Cannot remove completed recovery package.');
  RequireRuntime(DeleteFile(Root + '\journal.ini'), 'Cannot remove completed recovery journal.');
  RequireRuntime(DeleteFile(Root + '\complete'), 'Cannot remove completed recovery marker.');
  RequireRuntime(RemoveDir(Root), 'Unknown runtime recovery content.');
end;

procedure VerifyCompletedRuntime(Root: String);
var I: Integer; Name, Actual, Expected: String; Files: TStringList;
begin
  for I := 0 to 2 do begin
    Name := RecoveryMetadata(I);
    Expected := GetIniString('Next', Name, '', Root + '\journal.ini');
    Actual := RecoveryHash(RecoveryPath(ExpandConstant('{app}'), Name));
    RequireRuntime(Actual = Expected, 'Completed runtime metadata changed.');
  end;
  Files := TStringList.Create;
  Files.Sorted := True;
  try ReadFiles(Root + '\next\runtime-install.ini', Files, True);
  finally Files.Free; end;
end;

procedure ReadRecoveryInventory(PackageFile, Root: String; Previous, Files: TStringList);
var Startup, Modules, Directories: TStringList; Old: String;
begin
  Startup := TStringList.Create; Modules := TStringList.Create; Directories := TStringList.Create;
  try
    ReadStartup(PackageFile, Startup);
    Old := Root + '\previous\runtime-install.ini';
    if FileExists(Old) then begin
      CheckIdentity(PackageFile, Old, Startup, Root + '\previous');
      ReadModules(Old, Startup, Modules);
      ReadFiles(Old, Previous, True);
      ReadDirectories(Old, Directories, Previous);
    end;
    Files.Assign(Previous);
    ReadModules(PackageFile, Startup, Modules);
    ReadFiles(PackageFile, Files, False);
    ReadDirectories(PackageFile, Directories, Files);
  finally Directories.Free; Modules.Free; Startup.Free; end;
end;

procedure RecoverRuntimeInstall(PackageFile: String);
var Root, Journal, Name, Target: String; Names, Previous, Files: TStringList; I, P: Integer;
begin
  Root := RecoveryRoot;
  RequireRuntime(not FileExists(Root), 'Runtime recovery path is not a directory.');
  if not DirExists(Root) then Exit;
  Names := TStringList.Create; Previous := TStringList.Create; Files := TStringList.Create;
  Previous.Sorted := True; Files.Sorted := True;
  try
    CheckRecoveryJournal(Root, Names);
    Journal := Root + '\journal.ini';
    if FileExists(Root + '\complete') then begin
      RequireRuntime(RecoveryHash(Journal) = Trim(GetIniString('Commit', 'Journal', '', Root + '\complete')),
        'Invalid runtime recovery completion marker.');
      VerifyCompletedRuntime(Root);
      FinishRuntimeJournal(Root, Names);
      RuntimeRecoveryActive := False;
      Exit;
    end;
    RequireRuntime(RecoveryHash(PackageFile) = GetIniString('State', 'Package', '', Journal),
      'An interrupted runtime update requires the same installer before another update.');
    CheckRecoveryTargets(Root, Names);
    RuntimeRecoveryActive := True;
    ReadRecoveryInventory(PackageFile, Root, Previous, Files);
    // All inputs are validated before any old metadata or added payload is changed.
    RestoreRuntimeMetadata(Root, Names);
    for I := 0 to Files.Count - 1 do begin
      P := Pos('|', Files[I]); Name := Copy(Files[I], 1, P - 1);
      if FileDigest(Previous, Name) = '' then begin
        Target := RuntimePath(Name);
        if FileExists(Target) then begin
          RequireRuntime(Lowercase(GetSHA256OfFile(Target)) = Copy(Files[I], P + 1, 64), 'Added runtime file changed during recovery.');
          RequireRuntime(DeleteFile(Target), 'Cannot remove an interrupted runtime addition.');
        end;
      end;
    end;
    Log('Interrupted runtime installation recovered; reinstalling verified additions.');
  finally Files.Free; Previous.Free; Names.Free; end;
end;

procedure CompleteRuntimeJournal(Root: String);
var Source, Temporary: String; Attempt: Integer;
begin
  Source := ExpandConstant('{tmp}\runtime-complete.ini');
  RecoverySave(Source, 'Commit', 'Journal', RecoveryHash(Root + '\journal.ini'));
  Attempt := 0;
  repeat
    Temporary := RecoveryPath(Root, 'commit-' + IntToStr(Attempt) + '.tmp');
    Attempt := Attempt + 1;
    RequireRuntime(Attempt <= 4096, 'Too many interrupted runtime completions.');
  until not FileExists(Temporary) and not DirExists(Temporary);
  RecoveryCopy(Source, Temporary);
  RequireRuntime(RecoveryMove(Temporary, RecoveryPath(Root, 'complete'), 8),
    'Cannot activate runtime recovery completion marker.');
end;

procedure CurStepChanged(Step: TSetupStep);
var Root: String; Names, Startup: TStringList;
begin
  if (Step <> ssDone) or not RuntimeRecoveryActive then Exit;
  Root := RecoveryRoot;
  Names := TStringList.Create; Startup := TStringList.Create;
  try
    CheckRecoveryJournal(Root, Names);
    ReadStartup(Root + '\package.ini', Startup);
    CheckIdentity(Root + '\package.ini', ExpandConstant('{app}\runtime-install.ini'), Startup, ExpandConstant('{app}'));
    VerifyCompletedRuntime(Root);
    CompleteRuntimeJournal(Root);
    FinishRuntimeJournal(Root, Names);
    RuntimeRecoveryActive := False;
  finally Startup.Free; Names.Free; end;
end;

function GetCustomSetupExitCode: Integer;
begin
  Result := 0;
  if RuntimeRecoveryActive then begin
    Log('Runtime installation finalization is incomplete; rerun the same installer.');
    Result := 1;
  end;
end;
