%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function lines = runtime_recovery_script(stage, payload, output, plan, options)
  lines = __nelsonc_runtime_script__(stage, payload, output, plan, options);
  pause = {'procedure PauseRecovery(Name: String);', 'var Marker: String;', 'begin', ...
    '  if ExpandConstant(''{param:RECOVERYPAUSE|}'') <> Name then Exit;', ...
    '  if (Name = ''payload'') and (ExtractFileName(CurrentFileName) <> ''value-b.txt'') then Exit;', ...
    '  Marker := ExpandConstant(''{param:RECOVERYMARKER|}'');', ...
    '  if not SaveStringToFile(Marker, ''PAUSED-'' + Name, False) then RaiseException(''Cannot create pause marker.'');', ...
    '  while not FileExists(Marker + ''.release'') do Sleep(25);', 'end;'};
  inserted = false;
  for k = 1:numel(lines)
    line = lines{k};
    phase = '';
    if startsWith(line, 'Source:')
      if contains(line, 'recursesubdirs'); phase = 'payload'; end
      if contains(line, '{tmp}\runtime-install.ini'); phase = 'ledger'; end
      if contains(line, '{tmp}\runtime.json'); phase = 'manifest'; end
      if contains(line, '{tmp}\modules.m'); phase = 'startup'; end
      if ~isempty(phase); line = [line, '; AfterInstall: PauseRecovery(''', phase, ''')']; end
    end
    line = strrep(line, 'FinishRuntimeJournal(Root, Names);', 'PauseRecovery(''commit''); FinishRuntimeJournal(Root, Names);');
    line = strrep(line, 'Root := Saved;', 'Root := Saved; PauseRecovery(''retired'');');
    line = strrep(line, 'RequireRuntime(RecoveryMove(Root, RecoveryRoot, 8)', ...
      'PauseRecovery(''activation-lock''); RequireRuntime(RecoveryMove(Root, RecoveryRoot, 8)');
    line = strrep(line, 'if FileExists(Target) then RequireRuntime(DeleteFile(Target), ''Cannot clean runtime recovery file.'');', ...
      'if FileExists(Target) then RequireRuntime(DeleteFile(Target), ''Cannot clean runtime recovery file.''); PauseRecovery(''cleanup'');');
    line = strrep(line, 'RestoreRuntimeMetadata(Root, Names);', ...
      'RestoreRuntimeMetadata(Root, Names); PauseRecovery(''restore'');');
    line = strrep(line, 'RequireRuntime(RecoveryMove(Temporary, Target, 9), ''Cannot restore runtime metadata.'');', ...
      'RequireRuntime(RecoveryMove(Temporary, Target, 9), ''Cannot restore runtime metadata.''); PauseRecovery(''metadata'');');
    line = strrep(line, 'RequireRuntime(RecoveryMove(Temporary, RecoveryPath(Root, ''complete''), 8)', ...
      'PauseRecovery(''completion''); RequireRuntime(RecoveryMove(Temporary, RecoveryPath(Root, ''complete''), 8)');
    if strcmp(line, '[Code]') && ~inserted
      line = strjoin([{line}, pause], char(10));
      inserted = true;
    end
    lines{k} = line;
  end
  lines = [lines, {
    '<event(''CurStepChanged'')>', 'procedure PauseAfterInstall(Step: TSetupStep);', 'begin', ...
    '  if Step = ssPostInstall then begin', ...
    '    PauseRecovery(''uninstall''); PauseRecovery(''finalization-failure'');', '  end;', 'end;'}];
end
