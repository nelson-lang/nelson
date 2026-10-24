%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Regression: opening several figures with a subtitle and then quitting used to
% segfault the webview desktop at module unload. finishWebviewModule() called
% webui_clean() on the evaluator thread while the main thread was still pumping
% the WebUI teardown (use-after-free plus a lock on a destroyed critical
% section), crashing ~90% of exits once six or more such figures were open.
% Only the desktop path (--file) exercises this; the headless --web path does
% not, so the crash must be reproduced through a real webview process.
%=============================================================================
addpath([modulepath('engine'), '/tests']);
%=============================================================================
nelson_web = [modulepath('nelson', 'bin'), '/nelson-webview'];
if ispc()
  nelson_web = [nelson_web, '.exe'];
end
if ~isfile(nelson_web)
  return
end
%=============================================================================
work_dir = tempname();
mkdir(work_dir);
cleanup_work = onCleanup(@() rmdir(work_dir, 's'));
%=============================================================================
% Sanity check: skip on a headless environment where the webview desktop cannot
% start at all (a single figure never triggers the teardown crash).
sanity_script = [work_dir, '/sanity.m'];
filewrite(sanity_script, sprintf('figure();\nexit(0);\n'));
[sanity_status, sanity_output] = run_nelson_cli(nelson_web, [' --file "', sanity_script, '"']);
if sanity_status ~= 0
  return
end
%=============================================================================
% Eight figures, each with a title and a subtitle, left open at quit.
repro_script = [work_dir, '/repro.m'];
filewrite(repro_script, sprintf(['for k = 1:8\n', ...
  '  figure();\n', ...
  '  plot((1:10) .^ 2);\n', ...
  '  title("t", "s");\n', ...
  'end\n', ...
  'exit(0);\n']));
[status, output] = run_nelson_cli(nelson_web, [' --file "', repro_script, '"']);
asserts.isequal(status, 0, output);
%=============================================================================
