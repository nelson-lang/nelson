%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A rejected command line must reach the caller on the error stream and end the
% process: nelson-gui has no console, and reporting through a modal dialog left
% it alive forever with no output at all.
%=============================================================================
if ~ispc()
  return
end
%=============================================================================
binpath = modulepath('nelson', 'bin');
nelson_gui = [binpath, '/nelson-gui'];
if isfile([nelson_gui, '.exe'])
  quoted_exe = ['"', replace(nelson_gui, '"', '\"'), '"'];
  % A mode-selector option is only valid for the generic 'nelson' launcher; passed
  % straight to the windowed nelson-gui binary it must be reported on the error
  % stream and end the process, not swallowed by a modal dialog.
  arguments = ' --quiet --no-ipc --cli';
  [status, output] = system([quoted_exe, arguments], 120);
  asserts.notEqual(status, 0);
  asserts.istrue(contains(output, 'Nelson:ProgramOptions:ModeSelector'), output);
end
%=============================================================================
