//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//============================================================================
procedure AddWindowsTerminalProfile(); forward;
function GetWindowsTerminalSettingsPath(): string; forward;
function ReadFileToString(const FileName: string): string; forward;
procedure WriteFileFromString(const FileName, Content: string); forward;
//=============================================================================
var
  SecondLicensePage: TOutputMsgMemoWizardPage;
  License2AcceptedRadio: TRadioButton;
  License2NotAcceptedRadio: TRadioButton;
//=============================================================================
const
  PF_AVX2_INSTRUCTIONS_AVAILABLE = 40;  // Windows constant for AVX2 support check
//=============================================================================
function IsProcessorFeaturePresent(feature: Integer): Boolean;
external 'IsProcessorFeaturePresent@kernel32.dll stdcall';
//=============================================================================
function IsAVX2Supported(): Boolean;
begin
  Result := IsProcessorFeaturePresent(PF_AVX2_INSTRUCTIONS_AVAILABLE);
end;
//=============================================================================
function IsSilentMode(): Boolean;
var
  j: Integer;
begin
  Result := False;
  for j := 1 to ParamCount do
  begin
      if CompareText(UpperCase(ParamStr(j)), '/VERYSILENT') = 0 then
      begin
        Result := True;
        Break;
      end;
      if CompareText(UpperCase(ParamStr(j)), '/SILENT') = 0 then
    begin
      Result := True;
      Break;
    end;
  end;
end;
//=============================================================================
procedure CheckLicense2Accepted(Sender: TObject);
begin
  WizardForm.NextButton.Enabled := License2AcceptedRadio.Checked;
end;
//=============================================================================
function CloneLicenseRadioButton(Source: TRadioButton): TRadioButton;
begin
  Result := TRadioButton.Create(WizardForm);
  Result.Parent := SecondLicensePage.Surface;
  Result.Caption := Source.Caption;
  Result.Left := Source.Left;
  Result.Top := Source.Top;
  Result.Width := Source.Width;
  Result.Height := Source.Height;
  Result.OnClick := @CheckLicense2Accepted;
end;
//=============================================================================
function FileReplaceString(const FileName, SearchString, ReplaceString: string):boolean;
var
  fileToEdit : TStrings;
  textToChange : string;
begin
  fileToEdit := TStringList.Create;

  try
    result := true;

    try
      fileToEdit.LoadFromFile(FileName);
      textToChange := fileToEdit.Text;

      if StringChangeEx(textToChange, SearchString, ReplaceString, True) > 0 then
        begin;
        fileToEdit.Text := textToChange;
        fileToEdit.SaveToFile(FileName);
      end;
    except
      result := false;
    end;
  finally
    fileToEdit.Free;
  end;
end;
//=============================================================================
function configureModuleFlag(const MODULE_NAME: string;const enableModule: boolean) : boolean;
begin
    result := false;
    if not enableModule then
    begin
        FileReplaceString(ExpandConstant('{app}') + '\' + 'modules' + '\' + 'modules.m', 
        '{''' + MODULE_NAME + ''', true', 
        '{''' + MODULE_NAME + ''', false');
        result := true;
    end;
end;
//=============================================================================
function configureModule(const COMPONENT_NAME, MODULE_NAME: string) : boolean;
begin
    result := configureModuleFlag(MODULE_NAME, WizardIsComponentSelected(COMPONENT_NAME));
end;
//=============================================================================
procedure updateModulesList();
var
  ModulesList: TStringList;
  I : Integer;

	begin;
    ModulesList := TStringList.Create;
    ModulesList.Add(ExpandConstant('{#COMPONENT_PARALLEL}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_MPI}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_DYNAMIC_LINK}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_MEX}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_SIO_CLIENT}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_SLICOT}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_CONTROL_SYSTEM}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_FFTW}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_GUI}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_QML_ENGINE}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_GRAPHICS}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_TEXT_EDITOR}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_TESTS_MANAGER}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_COM_ENGINE}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_VALIDATORS}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_DATA_ANALYSIS}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_IPC}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_SPECIAL_FUNCTIONS}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_AUDIO}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_TRIGONOMETRIC_FUNCTIONS}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_STATISTICS}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_POLYNOMIAL_FUNCTIONS}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_SIGNAL_PROCESSING}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_RANDOM}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_HELP_TOOLS}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_FILE_ARCHIVER}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_F2C}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_NIG}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_JSON}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_XML}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_WEBTOOLS}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_MATIO}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_HDF5}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_GEOMETRY}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_PYTHON_ENGINE}'));
      #ifdef WITH_JULIA_ENGINE
        ModulesList.Add(ExpandConstant('{#COMPONENT_JULIA_ENGINE}'));
      #else
        configureModuleFlag('julia_engine', False);
      #endif    
      for I := 0 to ModulesList.Count - 1 do
      begin;
        configureModule(ModulesList[I], AnsiLowercase(ModulesList[I]));
      end;
      ModulesList.Free;

    configureModule(ExpandConstant('{#COMPONENT_INTERNATIONALIZATION}'), 'localization');
    configureModule(ExpandConstant('{#COMPONENT_INTERNATIONALIZATION}'), 'characters_encoding');

	end;
//=============================================================================
procedure AfterNelsonInstall();
	var
		LanguageFileName : String;
		PreferencesDir : String;
		i : Integer;
		d : Integer;
		LanguageFileLines: TArrayOfString;
    InnosetupLanguage : String;
    Language : String;

	begin
    updateModulesList();
    i := 0;
    setArrayLength(LanguageFileLines, 5);
    for d := 0 to GetArrayLength(LanguageFileLines)-1 do
      begin
        LanguageFileLines[d] := '';
      end;
		PreferencesDir := ExpandConstant('{userappdata}') + '\' + 'Nelson' + '\' + ExpandConstant('{#APPLICATION_VERSION}');
		LanguageFileName := preferencesDir + '\' + 'nelson.conf';
		if not DirExists(preferencesDir) then
			begin
				ForceDirectories(preferencesDir);
			end;
		if not FileExists(LanguageFileName) then	 
			begin
        InnosetupLanguage := ExpandConstant('{language}');

				Language := 'en_US';
        if CompareText(InnosetupLanguage, 'english') = 0 then
          begin
            Language := 'en_US';
          end;
        if CompareText(InnosetupLanguage, 'french') = 0 then
          begin
            Language := 'fr_FR';
          end;
        LanguageFileLines[i] := '{"language":"' + Language + '"}'; i := i + 1;
				SaveStringsToFile(LanguageFileName, LanguageFileLines, False);
			end;
      AddWindowsTerminalProfile();
	end;
//=============================================================================
Procedure URLLabelOnClick(Sender: TObject);
var
  ErrorCode: Integer;
begin
  ShellExec('open', 'https://nelson-lang.github.io/nelson-website/', '', '', SW_SHOWNORMAL, ewNoWait, ErrorCode);
end;
//=============================================================================
Procedure InitializeWizard;
var
  URLLabel: TNewStaticText;
  LicenseFileName: string;
  LicenseFilePath: string;
 
begin
  URLLabel := TNewStaticText.Create(WizardForm);
  URLLabel.Caption := 'Nelson''s website';
  URLLabel.Cursor := crHand;
  URLLabel.OnClick := @URLLabelOnClick;
  URLLabel.Parent := WizardForm;
  
  URLLabel.Font.Style := URLLabel.Font.Style + [fsUnderline];
  URLLabel.Font.Color := clBlue;
  URLLabel.Top := WizardForm.ClientHeight - 1;
  URLLabel.Left := ScaleX(500);

  if not IsSilentMode() then
  begin
      SecondLicensePage :=
        CreateOutputMsgMemoPage(
          wpSelectComponents, SetupMessage(msgWizardLicense), SetupMessage(msgLicenseLabel),
          SetupMessage(msgLicenseLabel3), '');

      SecondLicensePage.RichEditViewer.Height := WizardForm.LicenseMemo.Height;

    LicenseFileName := 'gpl-3.0.md';
    ExtractTemporaryFile(LicenseFileName);
    LicenseFilePath := ExpandConstant('{tmp}\' + LicenseFileName);
    SecondLicensePage.RichEditViewer.Lines.LoadFromFile(LicenseFilePath);
      DeleteFile(LicenseFilePath);
    
      License2AcceptedRadio :=
        CloneLicenseRadioButton(WizardForm.LicenseAcceptedRadio);
      License2AcceptedRadio.Top := License2AcceptedRadio.Top + 77;

      License2NotAcceptedRadio :=
        CloneLicenseRadioButton(WizardForm.LicenseNotAcceptedRadio);
      License2NotAcceptedRadio.Top := License2NotAcceptedRadio.Top + 77;
  
    License2NotAcceptedRadio.Checked := True;
  end;
end;
//=============================================================================
procedure CurPageChanged(CurPageID: Integer);
begin
  if not IsSilentMode() then
  begin
    if CurPageID = SecondLicensePage.ID then
      begin
        CheckLicense2Accepted(nil);
      end;
  end;
end;
//=============================================================================
function ShouldSkipPage(PageID: Integer): Boolean;
begin
  Result := False;
  if not IsSilentMode() then
     begin
      if PageID = SecondLicensePage.ID then
        begin
    Result := True;
end;
     end;
end;
//=============================================================================
function NextButtonClick(CurPageID: Integer): Boolean;
  begin
    Result := true;

    if (CurPageID = wpWelcome) then
      begin
          if not Is64BitInstallMode then
          begin
    #ifndef NELSON_WOA64
      
            if IsWin64() and not IsSilentMode() then
begin
                SuppressibleMsgBox(CustomMessage('MESSAGEBOX_X64_VERSION_RECOMMANDED'), mbInformation, MB_OK, MB_OK );
              end;
    #endif
          end;
      end;

    if (CurPageId = wpSelectComponents) then
      begin

        if ( 
          (WizardIsComponentSelected( ExpandConstant('{#COMPONENT_GUI}') ) = false) and 
          ( (WizardIsComponentSelected(ExpandConstant('{#COMPONENT_QML_ENGINE}')) = true) or
          (WizardIsComponentSelected(ExpandConstant('{#COMPONENT_TEXT_EDITOR}')) = true) or 
          (WizardIsComponentSelected(ExpandConstant('{#COMPONENT_GRAPHICS}')) = true) )) then
          begin
              SuppressibleMsgBox( CustomMessage('MESSAGEBOX_GUI_REQUIRED'),
                mbError, MB_OK, MB_OK );
            Result := false;
          end;

        if ( 
          (WizardIsComponentSelected( ExpandConstant('{#COMPONENT_TESTS_MANAGER}') ) = false) and 
          ( (WizardIsComponentSelected(ExpandConstant('{#COMPONENT_UNIT_TESTS}')) = true))) then
          begin
              SuppressibleMsgBox( CustomMessage('MESSAGEBOX_TESTS_MANAGER_REQUIRED'),
                mbError, MB_OK, MB_OK );
            Result := false;
          end;

        if ( 
          (WizardIsComponentSelected( ExpandConstant('{#COMPONENT_DATA_ANALYSIS}') ) = false) and 
          ( (WizardIsComponentSelected(ExpandConstant('{#COMPONENT_VALIDATORS}')) = true))) then
          begin
              SuppressibleMsgBox( CustomMessage('MESSAGEBOX_DATA_ANALYSIS_REQUIRED'),
                mbError, MB_OK, MB_OK );
            Result := false;
          end;

        if ( 
          (WizardIsComponentSelected( ExpandConstant('{#COMPONENT_F2C}') ) = false) and 
          ( (WizardIsComponentSelected(ExpandConstant('{#COMPONENT_SLICOT}')) = true))) then
          begin
              SuppressibleMsgBox( CustomMessage('MESSAGEBOX_F2C_REQUIRED'),
                mbError, MB_OK, MB_OK );
            Result := false;
          end;

        if ( 
          (WizardIsComponentSelected( ExpandConstant('{#COMPONENT_JSON}') ) = false) and 
          ( (WizardIsComponentSelected(ExpandConstant('{#COMPONENT_WEBTOOLS}')) = true))) then
          begin
              SuppressibleMsgBox( CustomMessage('MESSAGEBOX_JSON_REQUIRED'),
                mbError, MB_OK, MB_OK );
            Result := false;
          end;

        if ( 
          (WizardIsComponentSelected( ExpandConstant('{#COMPONENT_HDF5}') ) = false) and 
          ( (WizardIsComponentSelected(ExpandConstant('{#COMPONENT_MATIO}')) = true))) then
          begin
              SuppressibleMsgBox( CustomMessage('MESSAGEBOX_HDF5_REQUIRED'),
                mbError, MB_OK, MB_OK );
            Result := false;
          end;

        if ( 
          (WizardIsComponentSelected( ExpandConstant('{#COMPONENT_SLICOT}') ) = false) and 
          ( (WizardIsComponentSelected(ExpandConstant('{#COMPONENT_CONTROL_SYSTEM}')) = true))) then
          begin
              SuppressibleMsgBox( CustomMessage('MESSAGEBOX_SLICOT_REQUIRED'),
                mbError, MB_OK, MB_OK );
            Result := false;
          end;

        if ( 
          (WizardIsComponentSelected( ExpandConstant('{#COMPONENT_XML}') ) = false) and 
          ( (WizardIsComponentSelected(ExpandConstant('{#COMPONENT_HELP_TOOLS}')) = true))) then
          begin
              SuppressibleMsgBox( CustomMessage('MESSAGEBOX_XML_REQUIRED'),
                mbError, MB_OK, MB_OK );
            Result := false;
          end;


      end;
  end;
//=============================================================================
function GetDefaultDirName(Param: string): string;
begin
  if IsAdminInstallMode then
  begin
    Result := ExpandConstant('{pf}');
  end
  else
  begin
    Result := ExpandConstant('{userpf}');
  end;
end;
//=============================================================================
function InitializeSetup: Boolean;
begin
#ifndef NELSON_WOA64
  if IsWin64() and not IsAVX2Supported then
  begin
    SuppressibleMsgBox(CustomMessage('MESSAGEBOX_AVX2_REQUIRED'), mbError, MB_OK, MB_OK);
    Abort;
    Result := False;
  end;
#endif
  Result := True;
end;
//=============================================================================
function AddNelsonProfile(const JsonContent: string): string;
var
  NelsonProfile: string;
  BPos, ProfilesPos: Integer;
begin
  Result := JsonContent;
  if (Pos('"name": "Nelson"', JsonContent) > 0) or (Pos('nelson-adv-cli.exe', LowerCase(JsonContent)) > 0) then Exit;

  NelsonProfile := #13#10 + '            {' + #13#10 +
                   '                "name": "Nelson",' + #13#10 +
                   '                "commandline": "%NELSON_RUNTIME_PATH%\\nelson-adv-cli.exe",' + #13#10 +
                   '                "startingDirectory": "%USERPROFILE%"' + #13#10 +
                   '            }';

  ProfilesPos := Pos('"list":', JsonContent);
  if ProfilesPos = 0 then ProfilesPos := Pos('"profiles":', JsonContent);
  
  if ProfilesPos > 0 then
  begin
    // Simple logic to find array start
    BPos := ProfilesPos;
    while (BPos < Length(JsonContent)) and (JsonContent[BPos] <> '[') do Inc(BPos);
    
    if JsonContent[BPos] = '[' then
      Insert(NelsonProfile + ',', Result, BPos + 1);
  end;
end;
//=============================================================================
procedure AddWindowsTerminalProfile();
var
  SettingsPath, SettingsContent, ModifiedContent: string;
begin
  SettingsPath := GetWindowsTerminalSettingsPath();
  if SettingsPath <> '' then
  begin
    SettingsContent := ReadFileToString(SettingsPath);
    if SettingsContent <> '' then
    begin
      ModifiedContent := AddNelsonProfile(SettingsContent);
      if ModifiedContent <> SettingsContent then
      begin
        WriteFileFromString(SettingsPath, ModifiedContent);
      end;
    end;
  end;
end;
//=============================================================================
function GetWindowsTerminalSettingsPath(): string;
var
  LocalAppData: string;
  Candidate: string;
begin
  Result := '';
  LocalAppData := ExpandConstant('{localappdata}');
  
  Candidate := LocalAppData + '\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState\settings.json';
  if FileExists(Candidate) then begin Result := Candidate; Exit; end;

  Candidate := LocalAppData + '\Packages\Microsoft.WindowsTerminalPreview_8wekyb3d8bbwe\LocalState\settings.json';
  if FileExists(Candidate) then begin Result := Candidate; Exit; end;
end;
//=============================================================================
function ReadFileToString(const FileName: string): string;
var
  SL: TStringList;
begin
  Result := '';
  if not FileExists(FileName) then Exit;
  SL := TStringList.Create;
  try
    SL.LoadFromFile(FileName);
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;
//=============================================================================
procedure WriteFileFromString(const FileName, Content: string);
var
  SL: TStringList;
begin
  ForceDirectories(ExtractFileDir(FileName));
  SL := TStringList.Create;
  try
    SL.Text := Content;
    SL.SaveToFile(FileName);
  finally
    SL.Free;
  end;
end;
//=============================================================================
function MakeUniqueBackupName(const BaseFileName: string): string;
var
  i: Integer;
  Candidate: string;
begin
  Candidate := BaseFileName + '.nelson.bak';
  i := 1;
  while FileExists(Candidate) do
  begin
    Candidate := BaseFileName + '.nelson.' + IntToStr(i) + '.bak';
    Inc(i);
  end;
  Result := Candidate;
end;
//=============================================================================
function RemoveNelsonProfile(const JsonContent: string): string;
var
  namePos: Integer;
  cmdPos: Integer;
  startPos: Integer;
  endPos: Integer;
  afterPos: Integer;
  beforePos: Integer;
begin
  Result := JsonContent;

  // Quick existence checks
  if (Pos(UpperCase('"name": "Nelson"'), UpperCase(JsonContent)) = 0) and (Pos('nelson-adv-cli.exe', LowerCase(JsonContent)) = 0) then
    Exit;

  // Prefer locating by name
  namePos := Pos(UpperCase('"name": "Nelson"'), UpperCase(JsonContent));
  if namePos = 0 then
    cmdPos := Pos('nelson-adv-cli.exe', LowerCase(JsonContent))
  else
    cmdPos := namePos;

  if cmdPos = 0 then Exit;

  // Find opening brace before position
  startPos := cmdPos;
  while (startPos > 0) and (JsonContent[startPos] <> '{') do Dec(startPos);
  if startPos <= 0 then Exit;

  // Find closing brace after position
  endPos := cmdPos;
  while (endPos <= Length(JsonContent)) and (JsonContent[endPos] <> '}') do Inc(endPos);
  if endPos > Length(JsonContent) then Exit;
  afterPos := endPos + 1;

  // Skip whitespace after
  while (afterPos <= Length(JsonContent)) and (JsonContent[afterPos] in [#9,#10,#13,#32]) do Inc(afterPos);

  // If there's a trailing comma after the object, remove it too
  if (afterPos <= Length(JsonContent)) and (JsonContent[afterPos] = ',') then
  begin
    Inc(afterPos);
  end
  else
  begin
    // Otherwise, if there's a comma before the object, remove it instead to keep JSON valid
    beforePos := startPos - 1;
    while (beforePos >= 1) and (JsonContent[beforePos] in [#9,#10,#13,#32]) do Dec(beforePos);
    if (beforePos >= 1) and (JsonContent[beforePos] = ',') then
    begin
      startPos := beforePos;
    end;
  end;

  Result := Copy(JsonContent, 1, startPos - 1) + Copy(JsonContent, afterPos, Length(JsonContent));
end;
//=============================================================================
procedure DeinitializeUninstall();
var
  SettingsPath: string;
  SettingsContent: string;
  ModifiedContent: string;
  BackupPath: string;
begin
  SettingsPath := GetWindowsTerminalSettingsPath();
  if SettingsPath = '' then
  begin
    Exit;
  end;

  SettingsContent := ReadFileToString(SettingsPath);
  if SettingsContent = '' then
  begin
    Exit;
  end;

  ModifiedContent := RemoveNelsonProfile(SettingsContent);
  if ModifiedContent <> SettingsContent then
  begin
    try
      BackupPath := MakeUniqueBackupName(SettingsPath);
      WriteFileFromString(BackupPath, SettingsContent);
    except
    end;

    try
      WriteFileFromString(SettingsPath, ModifiedContent);
    except
    end;
  end
  else
end;
//=============================================================================
