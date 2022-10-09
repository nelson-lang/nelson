//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================var
var
  SecondLicensePage: TOutputMsgMemoWizardPage;
  License2AcceptedRadio: TRadioButton;
  License2NotAcceptedRadio: TRadioButton;
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
function configureModule(const COMPONENT_NAME, MODULE_NAME: string) : boolean;
begin
    result := false;
    if not WizardIsComponentSelected(COMPONENT_NAME) then
      begin
        FileReplaceString(ExpandConstant('{app}') + '\' + 'modules' + '\' + 'modules.m', 
        '{''' + MODULE_NAME + ''', true', 
        '{''' + MODULE_NAME + ''', false');
        result := true;
      end
end;
//=============================================================================
procedure updateModulesList();
var
    ModulesList: TStringList;
	  I : Integer;

	begin;
    ModulesList := TStringList.Create;
    ModulesList.Add('PARALLEL');
    ModulesList.Add('MPI');
    ModulesList.Add('DYNAMIC_LINK');
    ModulesList.Add('MEX');
    ModulesList.Add('SIO_CLIENT');
    ModulesList.Add('SLICOT');
    ModulesList.Add('FFTW');
    ModulesList.Add('GUI');
    ModulesList.Add('HELP_BROWSER');
    ModulesList.Add('QML_ENGINE');
    ModulesList.Add('GRAPHICS');
    ModulesList.Add('TEXT_EDITOR');
    ModulesList.Add('TESTS_MANAGER');
    ModulesList.Add('COM_ENGINE');
    ModulesList.Add('VALIDATORS');
    ModulesList.Add('DATA_ANALYSIS');
    ModulesList.Add('IPC');

    for I := 0 to ModulesList.Count - 1 do
      begin;
         configureModule(ModulesList[I], AnsiLowercase(ModulesList[I]));
      end;
    ModulesList.Free;

    configureModule('INTERNALIZATION', 'localization');
    configureModule('INTERNALIZATION', 'i18n');
    configureModule('INTERNALIZATION', 'characters_encoding');

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
	end;

#IFDEF UNICODE
  #DEFINE AW "W"
#ELSE
  #DEFINE AW "A"
#ENDIF
type
  INSTALLSTATE = Longint;
const
  INSTALLSTATE_INVALIDARG = -2;  // An invalid parameter was passed to the function.
  INSTALLSTATE_UNKNOWN = -1;     // The product is neither advertised or installed.
  INSTALLSTATE_ADVERTISED = 1;   // The product is advertised but not installed.
  INSTALLSTATE_ABSENT = 2;       // The product is installed for a different user.
  INSTALLSTATE_DEFAULT = 5;      // The product is installed for the current user.

  // Visual C++ 2017 Redistributable 14.10.25017
  VC_2017_REDIST_X86_MIN = '{582EA838-9199-3518-A05C-DB09462F68EC}';
  VC_2017_REDIST_X64_MIN = '{8D4F7A6D-6B81-3DC8-9C21-6008E4866727}';

  VC_2017_REDIST_X86_ADD = '{58b3beca-b999-4f6f-a48c-81681136a620}';
  VC_2017_REDIST_X64_ADD = '{e9d78d68-c26c-4da7-9158-99355d8ef3ad}';
//=============================================================================
function MsiQueryProductState(szProduct: string): INSTALLSTATE; 
  external 'MsiQueryProductState{#AW}@msi.dll stdcall';
//=============================================================================
function VCVersionInstalled(const ProductID: string): Boolean;
begin
  Result := MsiQueryProductState(ProductID) = INSTALLSTATE_DEFAULT;
end;
//=============================================================================
function VCRedistNeedsInstall: Boolean;
begin
#ifdef NELSON_X64
  Result := not (VCVersionInstalled(VC_2017_REDIST_X64_MIN));
#else
  Result := not (VCVersionInstalled(VC_2017_REDIST_X86_MIN));
#endif
end;
//=============================================================================
Procedure URLLabelOnClick(Sender: TObject);
var
  ErrorCode: Integer;
begin
  ShellExec('open', 'https://nelson-numerical-software.github.io/nelson-website/', '', '', SW_SHOWNORMAL, ewNoWait, ErrorCode);
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
      Log ('Not VerySilent')
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
          Result := not WizardIsComponentSelected('SLICOT');
        end;
     end;
end;
//=============================================================================
function NextButtonClick(CurPageID: Integer): Boolean;
  Var
    bRes : Boolean;

  begin
    Result := true;

    if (CurPageId = wpSelectComponents) then
      begin

        if ( 
          (IsComponentSelected( ExpandConstant('{#COMPONENT_GUI}') ) = false) and 
          ( (IsComponentSelected(ExpandConstant('{#COMPONENT_QML_ENGINE}')) = true) or
          (IsComponentSelected(ExpandConstant('{#COMPONENT_TEXT_EDITOR}')) = true) or 
          (IsComponentSelected(ExpandConstant('{#COMPONENT_HELP_BROWSER}')) = true) or  
          (IsComponentSelected(ExpandConstant('{#COMPONENT_GRAPHICS}')) = true) )) then
          begin
              SuppressibleMsgBox( CustomMessage('MESSAGEBOX_GUI_REQUIRED'),
                mbError, MB_OK, MB_OK );
            Result := false;
          end;

        if ( 
          (IsComponentSelected( ExpandConstant('{#COMPONENT_TESTS_MANAGER}') ) = false) and 
          ( (IsComponentSelected(ExpandConstant('{#COMPONENT_UNIT_TESTS}')) = true))) then
          begin
              SuppressibleMsgBox( CustomMessage('MESSAGEBOX_TESTS_MANAGER_REQUIRED'),
                mbError, MB_OK, MB_OK );
            Result := false;
          end;

        if ( 
          (IsComponentSelected( ExpandConstant('{#COMPONENT_HELP_BROWSER}') ) = false) and 
          ( (IsComponentSelected(ExpandConstant('{#COMPONENT_HELP_FILES}')) = true))) then
          begin
              SuppressibleMsgBox( CustomMessage('MESSAGEBOX_HELP_BROWSER_REQUIRED'),
                mbError, MB_OK, MB_OK );
            Result := false;
          end;

        if ( 
          (IsComponentSelected( ExpandConstant('{#COMPONENT_DATA_ANALYSIS}') ) = false) and 
          ( (IsComponentSelected(ExpandConstant('{#COMPONENT_VALIDATORS}')) = true))) then
          begin
              SuppressibleMsgBox( CustomMessage('MESSAGEBOX_DATA_ANALYSIS_REQUIRED'),
                mbError, MB_OK, MB_OK );
            Result := false;
          end;


      end;
  end;