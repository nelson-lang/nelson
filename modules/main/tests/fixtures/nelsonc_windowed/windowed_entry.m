%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function windowed_entry(destination, mode, varargin)
  % Inspect the host console without redistributing an operating-system DLL.
  library = dlopen(fullfile(getenv('SystemRoot'), 'System32', 'kernel32.dll'));
  console = dlsym(library, 'GetConsoleWindow', 'uint64', {});
  report = struct('console', dlcall(console), 'arguments', {varargin}, ...
    'directory', pwd(), 'archive', ctfroot(), 'deployed', isdeployed(), 'value', sum((1:100) .^ 2));
  filewrite(destination, jsonencode(report));
  fprintf('WINDOWED_ENTRY_OK\n');
  pause(0.2);
  if strcmp(mode, 'error')
    error('WINDOWED_EXPECTED_ERROR');
  elseif strcmp(mode, 'exit')
    exit(7);
  end
end
