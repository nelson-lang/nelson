%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function cases = run_normalizeCases(raw)
  rawCases = run_resultCases(raw);
  cases = struct([]);
  for k = 1:length(rawCases)
    c = rawCases(k);
    attempts = 1;
    if isfield(c, 'attempts')
      attempts = c.attempts;
    end
    metadata = run_normalizedCaseMetadata(c);
    result = struct('id', c.name, 'file', metadata.file, 'module', metadata.module, ...
      'kind', metadata.kind, 'mode', metadata.mode, 'weight', metadata.weight, ...
      'outcome', run_normalizeStatus(c), ...
      'duration', c.time, 'message', run_messageText(c), 'stdout', run_stdoutText(c), ...
      'stderr', run_stderrText(c), 'exit_code', run_exitCode(c), 'attempts', attempts, ...
      'artifacts', {{}}, 'diagnostics', run_nativeDiagnostics(c));
    cases = [cases; result];
  end
end
%=============================================================================
function metadata = run_normalizedCaseMetadata(testCase)
  filename = run_normalizedCaseFilename(testCase);
  moduleName = discover_moduleNameFromFile(filename);
  kind = discover_caseKind(testCase.name);
  mode = run_normalizedCaseMode(testCase);
  weight = run_normalizedCaseWeight(testCase);
  metadata = struct('file', filename, 'module', moduleName, ...
    'kind', kind, 'mode', mode, 'weight', weight);
end
%=============================================================================
function filename = run_normalizedCaseFilename(testCase)
  filename = '';
  if isfield(testCase, 'filename')
    filename = testCase.filename;
  end
end
%=============================================================================
function mode = run_normalizedCaseMode(testCase)
  mode = 'cli';
  if ~isfield(testCase, 'options')
    return
  end
  if run_normalizedOptionEnabled(testCase.options, 'gui_mode')
    mode = 'gui';
  elseif run_normalizedOptionEnabled(testCase.options, 'adv_cli_mode')
    mode = 'adv_cli';
  end
end
%=============================================================================
function tf = run_normalizedOptionEnabled(options, name)
  tf = isfield(options, name) && logical(options.(name));
end
%=============================================================================
function weight = run_normalizedCaseWeight(testCase)
  weight = 1;
  if isfield(testCase, 'weight')
    weight = double(testCase.weight);
  end
end
%=============================================================================
