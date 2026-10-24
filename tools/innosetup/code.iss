//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//============================================================================
#include "compiler_dependencies.iss"
procedure AddWindowsTerminalProfile(); forward;
procedure RemoveWindowsTerminalProfile(); forward;
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
    if CompareText(ParamStr(j), '/VERYSILENT') = 0 then
    begin
      Result := True;
      Break;
    end;
    if CompareText(ParamStr(j), '/SILENT') = 0 then
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
function ModuleNameFromComponent(const Component: string): string;
// Components live under category parents ("grp_<cat>\<NAME>"); the module name
// is the lowercased leaf, so strip everything up to the last backslash.
var
  I: Integer;
  Name: string;
begin
  Name := Component;
  for I := Length(Name) downto 1 do
  begin
    if Name[I] = '\' then
    begin
      Name := Copy(Name, I + 1, Length(Name) - I);
      Break;
    end;
  end;
  Result := AnsiLowercase(Name);
end;
//=============================================================================
procedure updateModulesList();
var
  ModulesList: TStringList;
  I: Integer;
begin
  ModulesList := TStringList.Create;
  try
    ModulesList.Add(ExpandConstant('{#COMPONENT_PARALLEL}'));
#ifndef NELSON_WOA64
    ModulesList.Add(ExpandConstant('{#COMPONENT_MPI}'));
#else
    configureModuleFlag('mpi', False);
#endif
    ModulesList.Add(ExpandConstant('{#COMPONENT_DYNAMIC_LINK}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_MEX}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_SLICOT}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_CONTROL_SYSTEM}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_FFTW}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_GUI}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_WEBVIEW}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_QML_ENGINE}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_GRAPHICS}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_TEXT_EDITOR}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_TESTS_MANAGER}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_VALIDATORS}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_DATA_ANALYSIS}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_CATEGORICAL}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_IPC}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_SPECIAL_FUNCTIONS}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_AUDIO}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_TRIGONOMETRIC_FUNCTIONS}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_STATISTICS}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_POLYNOMIAL_FUNCTIONS}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_SIGNAL_PROCESSING}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_RANDOM}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_OPTIMIZATION}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_HELP_TOOLS}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_FILE_ARCHIVER}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_F2C}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_JSON}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_AI}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_XML}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_WEBTOOLS}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_MATIO}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_HDF5}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_NETCDF}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_SPREADSHEET}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_PARQUET}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_IMAGE_PROCESSING}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_GEOMETRY}'));
    ModulesList.Add(ExpandConstant('{#COMPONENT_PYTHON_ENGINE}'));
#ifdef WITH_JULIA_ENGINE
    ModulesList.Add(ExpandConstant('{#COMPONENT_JULIA_ENGINE}'));
#else
    configureModuleFlag('julia_engine', False);
#endif
#ifdef WITH_GPU_ENGINE
    ModulesList.Add(ExpandConstant('{#COMPONENT_GPU_ENGINE}'));
#else
    configureModuleFlag('gpu_engine', False);
#endif
    ModulesList.Add(ExpandConstant('{#COMPONENT_ODE_SOLVERS}'));
    for I := 0 to ModulesList.Count - 1 do
    begin
      configureModule(ModulesList[I], ModuleNameFromComponent(ModulesList[I]));
    end;
  finally
    ModulesList.Free;
  end;

  configureModule(ExpandConstant('{#COMPONENT_INTERNATIONALIZATION}'), 'localization');
  configureModule(ExpandConstant('{#COMPONENT_INTERNATIONALIZATION}'), 'characters_encoding');

  // preferences stores its data through HDF5, so it ships with the HDF5
  // component. Its module name does not match the component leaf ('hdf5'), so the
  // loop above does not cover it: disable its startup loader explicitly when HDF5
  // is not installed, otherwise startup.m fails on the missing loader.m.
  configureModule(ExpandConstant('{#COMPONENT_HDF5}'), 'preferences');

  // nflow_* modules share a single NFLOW component
  configureModuleFlag('nflow_blocks', WizardIsComponentSelected(ExpandConstant('{#COMPONENT_NFLOW}')));
  configureModuleFlag('nflow_engine', WizardIsComponentSelected(ExpandConstant('{#COMPONENT_NFLOW}')));
  configureModuleFlag('nflow_fmi', WizardIsComponentSelected(ExpandConstant('{#COMPONENT_NFLOW}')));
  configureModuleFlag('nflow_gui', WizardIsComponentSelected(ExpandConstant('{#COMPONENT_NFLOW}')));

  // These modules ship under a component whose leaf name differs from the module
  // name, so the loop above does not configure them: graphics_io follows the
  // GRAPHICS component, nmm_gui follows WEBVIEW.
  configureModuleFlag('graphics_io', WizardIsComponentSelected(ExpandConstant('{#COMPONENT_GRAPHICS}')));
  configureModuleFlag('nmm_gui', WizardIsComponentSelected(ExpandConstant('{#COMPONENT_WEBVIEW}')));
  configureModule(ExpandConstant('{#COMPONENT_COMPILER}'), 'compiler');

  // history_manager's core library is linked by the terminal, but its builtin
  // layer only serves the interactive history of the advanced CLI and the GUI
  // (both shipped under the GUI component); a CLI-only install does not load it.
  configureModuleFlag('history_manager', WizardIsComponentSelected(ExpandConstant('{#COMPONENT_GUI}')));

  // Comfort modules made optional so a minimal install is the strict numerical
  // core. text_completion keeps its statically-linked DLL through the no- stub,
  // so only the module (loader + .m functions) is dropped when deselected;
  // assert_functions has no importer and is dropped entirely.
  configureModule(ExpandConstant('{#COMPONENT_TEXT_COMPLETION}'), 'text_completion');
  configureModule(ExpandConstant('{#COMPONENT_ASSERT_FUNCTIONS}'), 'assert_functions');
end;
//=============================================================================
procedure AfterNelsonInstall();
var
  LanguageFileName: String;
  PreferencesDir: String;
  LanguageFileLines: TArrayOfString;
  InnosetupLanguage: String;
  Language: String;
begin
  updateModulesList();

  SetArrayLength(LanguageFileLines, 1);
  PreferencesDir := ExpandConstant('{userappdata}') + '\' + 'Nelson' + '\' + ExpandConstant('{#APPLICATION_VERSION}');
  LanguageFileName := PreferencesDir + '\' + 'nelson.conf';
  if not DirExists(PreferencesDir) then
  begin
    ForceDirectories(PreferencesDir);
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
    LanguageFileLines[0] := '{"language":"' + Language + '"}';
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
  VersionLabel: TNewStaticText;
  RadioHeight: Integer;
  RadioGap: Integer;
begin
  // Footer labels live in the bottom button bar, aligned to the left so they
  // never overlap the right-aligned Back / Next / Cancel buttons. They are
  // vertically centered on the Cancel button and anchored bottom-left so they
  // keep their place when the (resizable) wizard is resized.

  // Website link at the bottom-left of the wizard
  URLLabel := TNewStaticText.Create(WizardForm);
  URLLabel.Caption := 'nelson-lang.github.io/nelson-website';
  URLLabel.Cursor := crHand;
  URLLabel.OnClick := @URLLabelOnClick;
  URLLabel.Parent := WizardForm;
  URLLabel.Font.Style := URLLabel.Font.Style + [fsUnderline];
  URLLabel.Font.Color := clBlue;
  URLLabel.Anchors := [akLeft, akBottom];
  URLLabel.Top := WizardForm.CancelButton.Top +
    (WizardForm.CancelButton.Height - URLLabel.Height) div 2;
  URLLabel.Left := ScaleX(16);

  // Version label, right after the link, same baseline
  VersionLabel := TNewStaticText.Create(WizardForm);
  VersionLabel.Caption := 'v' + ExpandConstant('{#APPLICATION_VERSION}');
  VersionLabel.Parent := WizardForm;
  VersionLabel.Font.Color := clGray;
  VersionLabel.Anchors := [akLeft, akBottom];
  VersionLabel.Top := WizardForm.CancelButton.Top +
    (WizardForm.CancelButton.Height - VersionLabel.Height) div 2;
  VersionLabel.Left := URLLabel.Left + URLLabel.Width + ScaleX(12);

  if not IsSilentMode() then
  begin
    SecondLicensePage :=
      CreateOutputMsgMemoPage(
        wpSelectComponents, SetupMessage(msgWizardLicense), SetupMessage(msgLicenseLabel),
        SetupMessage(msgLicenseLabel3), '');

    RadioHeight := WizardForm.LicenseAcceptedRadio.Height;
    RadioGap := ScaleY(6);

    // Shrink the license viewer to leave room for the two accept / decline
    // radios below it, so their positions are derived from the page layout
    // instead of a magic pixel offset (robust under DPI and resizing).
    SecondLicensePage.RichEditViewer.Height :=
      SecondLicensePage.SurfaceHeight - (2 * RadioHeight + 3 * RadioGap);

    LicenseFileName := 'gpl-3.0.md';
    ExtractTemporaryFile(LicenseFileName);
    LicenseFilePath := ExpandConstant('{tmp}\' + LicenseFileName);
    SecondLicensePage.RichEditViewer.Lines.LoadFromFile(LicenseFilePath);
    DeleteFile(LicenseFilePath);

    License2AcceptedRadio := CloneLicenseRadioButton(WizardForm.LicenseAcceptedRadio);
    License2AcceptedRadio.Left := SecondLicensePage.RichEditViewer.Left;
    License2AcceptedRadio.Width := SecondLicensePage.RichEditViewer.Width;
    License2AcceptedRadio.Top :=
      SecondLicensePage.RichEditViewer.Top + SecondLicensePage.RichEditViewer.Height + RadioGap;

    License2NotAcceptedRadio := CloneLicenseRadioButton(WizardForm.LicenseNotAcceptedRadio);
    License2NotAcceptedRadio.Left := SecondLicensePage.RichEditViewer.Left;
    License2NotAcceptedRadio.Width := SecondLicensePage.RichEditViewer.Width;
    License2NotAcceptedRadio.Top :=
      License2AcceptedRadio.Top + RadioHeight + RadioGap;

    License2NotAcceptedRadio.Checked := True;
  end;
end;
//=============================================================================
procedure CurPageChanged(CurPageID: Integer);
begin
  if CurPageID = wpInstalling then
  begin
    // Hide the scrolling full-path label during installation; the per-phase
    // StatusMsg messages give readable progress instead of raw file paths.
    WizardForm.FilenameLabel.Visible := False;
  end;
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
procedure EnsureDependency(const RequiredComponent, DependentComponent: string);
// Auto-select RequiredComponent when DependentComponent is selected, instead of
// blocking with an error. WizardSelectComponents adds to the current selection
// (it leaves unspecified components unchanged) and also checks the required
// component's category parent.
begin
  if WizardIsComponentSelected(ExpandConstant(DependentComponent)) and
     (not WizardIsComponentSelected(ExpandConstant(RequiredComponent))) then
  begin
    WizardSelectComponents(ExpandConstant(RequiredComponent));
  end;
end;
//=============================================================================
function NextButtonClick(CurPageID: Integer): Boolean;
begin
  Result := True;
  if (CurPageID = wpSelectComponents) then
  begin
    // Missing dependencies are auto-selected instead of blocking with an error.
    // GUI is required by QML_ENGINE, TEXT_EDITOR and GRAPHICS.
    EnsureDependency('{#COMPONENT_GUI}', '{#COMPONENT_QML_ENGINE}');
    EnsureDependency('{#COMPONENT_GUI}', '{#COMPONENT_TEXT_EDITOR}');
    EnsureDependency('{#COMPONENT_GUI}', '{#COMPONENT_GRAPHICS}');
    // WEBVIEW is required by WEB_GUI and NFLOW.
    EnsureDependency('{#COMPONENT_WEBVIEW}', '{#COMPONENT_WEB_GUI}');
    EnsureDependency('{#COMPONENT_WEBVIEW}', '{#COMPONENT_NFLOW}');
    // NFLOW (nflow_fmi) statically links the file archiver to unpack FMUs, so it
    // cannot run against the stub shipped when FILE_ARCHIVER is deselected.
    EnsureDependency('{#COMPONENT_FILE_ARCHIVER}', '{#COMPONENT_NFLOW}');
    // The nflow engine builtin statically links the ode_solvers library (CVODES/IDAS
    // solver backends), so NFLOW cannot load unless ODE_SOLVERS is installed.
    EnsureDependency('{#COMPONENT_ODE_SOLVERS}', '{#COMPONENT_NFLOW}');
    // MEX builds MEX files with the dynamic_link toolchain and its module loader
    // requires both components, so MEX pulls in DYNAMIC_LINK.
    EnsureDependency('{#COMPONENT_DYNAMIC_LINK}', '{#COMPONENT_MEX}');
    // The mex builtin statically links the validators library, so it cannot load
    // unless VALIDATORS is installed.
    EnsureDependency('{#COMPONENT_VALIDATORS}', '{#COMPONENT_MEX}');
    // The graphics library statically links the json library (used to serialize
    // figures), so GRAPHICS cannot load unless JSON is installed.
    EnsureDependency('{#COMPONENT_JSON}', '{#COMPONENT_GRAPHICS}');
    // Simple 1-to-1 dependencies.
    EnsureDependency('{#COMPONENT_TESTS_MANAGER}', '{#COMPONENT_UNIT_TESTS}');
    // The test files rely on the assert_* helpers from assert_functions.
    EnsureDependency('{#COMPONENT_ASSERT_FUNCTIONS}', '{#COMPONENT_TESTS_MANAGER}');
    EnsureDependency('{#COMPONENT_DATA_ANALYSIS}', '{#COMPONENT_VALIDATORS}');
    EnsureDependency('{#COMPONENT_DATA_ANALYSIS}', '{#COMPONENT_CATEGORICAL}');
    EnsureDependency('{#COMPONENT_JSON}', '{#COMPONENT_WEBTOOLS}');
    EnsureDependency('{#COMPONENT_JSON}', '{#COMPONENT_AI}');
    EnsureDependency('{#COMPONENT_HDF5}', '{#COMPONENT_MATIO}');
    EnsureDependency('{#COMPONENT_XML}', '{#COMPONENT_HELP_TOOLS}');
    // CONTROL_SYSTEM needs SLICOT, which needs F2C; keep this order so the whole
    // chain is satisfied in a single pass.
    EnsureDependency('{#COMPONENT_SLICOT}', '{#COMPONENT_CONTROL_SYSTEM}');
    EnsureDependency('{#COMPONENT_F2C}', '{#COMPONENT_SLICOT}');
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
  Result := True;
#ifndef NELSON_WOA64
  if IsWin64() and not IsAVX2Supported then
  begin
    SuppressibleMsgBox(CustomMessage('MESSAGEBOX_AVX2_REQUIRED'), mbError, MB_OK, MB_OK);
    Abort;
    Result := False;
  end;
#endif
end;
//=============================================================================
procedure AddWindowsTerminalProfile();
var
  FragmentDir: string;
  FragmentFile: string;
  Lines: TArrayOfString;
begin
  FragmentDir :=
    ExpandConstant('{localappdata}') +
    '\Microsoft\Windows Terminal\Fragments\Nelson';

  FragmentFile := FragmentDir + '\Nelson.json';

  if not DirExists(FragmentDir) then
  begin
    ForceDirectories(FragmentDir);
  end;

  SetArrayLength(Lines, 10);

  Lines[0] := '{';
  Lines[1] := '  "profiles": [';
  Lines[2] := '    {';
  Lines[3] := '      "name": "Nelson",';
  Lines[4] := '      "commandline": "%NELSON_RUNTIME_PATH%\\nelson-adv-cli.exe",';
  Lines[5] := '      "startingDirectory": "%USERPROFILE%",';
  Lines[6] := '      "icon": "%NELSON_RUNTIME_PATH%\\nelson.ico"';
  Lines[7] := '    }';
  Lines[8] := '  ]';
  Lines[9] := '}';

  SaveStringsToFile(FragmentFile, Lines, False);
end;
//=============================================================================
procedure RemoveWindowsTerminalProfile();
var
  FragmentFile: string;
begin
  FragmentFile :=
    ExpandConstant('{localappdata}') +
    '\Microsoft\Windows Terminal\Fragments\Nelson\Nelson.json';

  if FileExists(FragmentFile) then
  begin
    DeleteFile(FragmentFile);
  end;
end;
//=============================================================================
procedure DeinitializeUninstall();
begin
  RemoveWindowsTerminalProfile();
end;
//=============================================================================
procedure RemoveUserPreferences();
var
  PreferencesDir: string;
  NelsonRootDir: string;
begin
  PreferencesDir :=
    ExpandConstant('{userappdata}') + '\Nelson\' + ExpandConstant('{#APPLICATION_VERSION}');
  if not DirExists(PreferencesDir) then
    Exit;
  // Default answer is "No" so a silent uninstall never deletes user data.
  if SuppressibleMsgBox(CustomMessage('UNINSTALL_REMOVE_PREFERENCES'),
       mbConfirmation, MB_YESNO or MB_DEFBUTTON2, IDNO) = IDYES then
  begin
    DelTree(PreferencesDir, True, True, True);
    // Remove the parent Nelson folder too, but only if no other version remains.
    NelsonRootDir := ExpandConstant('{userappdata}') + '\Nelson';
    RemoveDir(NelsonRootDir);
  end;
end;
//=============================================================================
procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
begin
  if CurUninstallStep = usPostUninstall then
  begin
    RemoveUserPreferences();
  end;
end;
//=============================================================================
