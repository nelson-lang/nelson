//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
function CompilerDependencyError: String;
begin
  Result := '';
  if WizardIsComponentSelected('{#COMPONENT_COMPILER}') and
     not WizardIsComponentSelected('{#COMPONENT_FILE_ARCHIVER} and {#COMPONENT_DYNAMIC_LINK} and {#COMPONENT_JSON}') then
    Result := CustomMessage('MESSAGEBOX_COMPILER_DEPENDENCIES');
end;

<event('NextButtonClick')>
function CheckCompilerComponents(CurPageID: Integer): Boolean;
var
  ErrorMessage: String;
begin
  Result := True;
  if CurPageID <> wpSelectComponents then Exit;
  ErrorMessage := CompilerDependencyError();
  if ErrorMessage <> '' then
  begin
    SuppressibleMsgBox(ErrorMessage, mbError, MB_OK, MB_OK);
    Result := False;
  end;
end;

<event('PrepareToInstall')>
function PrepareCompilerModule(var NeedsRestart: Boolean): String;
begin
  Result := CompilerDependencyError();
end;
//=============================================================================
