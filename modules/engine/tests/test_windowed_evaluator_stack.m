%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~ismac()
  return
end
%=============================================================================
nelsonGui = [modulepath('nelson', 'bin'), '/nelson-gui'];
if ~isfile(nelsonGui)
  return
end
%=============================================================================
functionName = ['windowed_stack_probe_', strrep(createGUID(), '-', '_')];
functionDirectory = [tempdir()];
functionPath = [functionDirectory, '/', functionName, '.m'];
functionText = [
  "function value = " + functionName + "(depth)"
  "  if depth == 0"
  "    value = 0;"
  "  else"
  "    value = 1 + " + functionName + "(depth - 1);"
  "  end"
  "end"
];
filewrite(functionPath, functionText);
cleanupFunction = onCleanup(@() rmfile(functionPath));
%=============================================================================
quotedDirectory = strrep(functionDirectory, '''', '''''');
command = [' --quiet --no-user-startup --no-ipc --execute "addpath(''', ...
  quotedDirectory, '''); assert(', functionName, '(100) == 100);"'];
[status, output] = run_nelson_cli(nelsonGui, command);
assert_isequal(status, 0, output);
%=============================================================================
