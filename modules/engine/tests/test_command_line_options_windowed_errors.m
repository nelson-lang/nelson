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
% process: nelson-gui and nelson-webview have no console, and reporting through
% a modal dialog left them alive forever with no output at all.
%=============================================================================
if ~ispc()
  return
end
%=============================================================================
binpath = modulepath('nelson', 'bin');
%=============================================================================
nelson_gui = [binpath, '/nelson-gui'];
if isfile([nelson_gui, '.exe'])
  quoted_exe = ['"', replace(nelson_gui, '"', '\"'), '"'];
  arguments = ' --quiet --no-ipc --url=127.0.0.1 -e "quit"';
  [status, output] = system([quoted_exe, arguments], 120);
  asserts.notEqual(status, 0);
  asserts.istrue(contains(output, 'Nelson:ProgramOptions:WebModeRequired'), output);
end
%=============================================================================
nelson_webview = [binpath, '/nelson-webview'];
if isfile([nelson_webview, '.exe'])
  quoted_exe = ['"', replace(nelson_webview, '"', '\"'), '"'];
  arguments = ' --quiet --no-ipc --file-ipc dummy.m';
  [status, output] = system([quoted_exe, arguments], 120);
  asserts.notEqual(status, 0);
  asserts.istrue(contains(output, 'Nelson:ProgramOptions:GuiModeRequired'), output);
end
%=============================================================================
