%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A desktop session that quits with a figure window open must end: a stuck
% exit leaves a windowless nelson-gui process holding every DLL of bin/.
%=============================================================================
if ~ispc()
  return
end
%=============================================================================
nelson_exe = [modulepath('nelson', 'bin'), '/nelson-gui'];
if ~isfile([nelson_exe, '.exe'])
  return
end
quoted_exe = ['"', replace(nelson_exe, '"', '\"'), '"'];
%=============================================================================
% exit requested while a Qt figure window is open
probe_file = [tempname(), '.txt'];
nelson_command = ['figure(); plot(1:10); title(''exit''); drawnow(); ', ...
  'fid = fopen(''', replace(probe_file, '\', '/'), ''', ''wt''); ', ...
  'fprintf(fid, ''alive''); fclose(fid); exit(7)'];
arguments = [' --quiet --no-ipc --no-user-startup --execute="', nelson_command, '"'];
[status, output] = system([quoted_exe, arguments], 180);
diagnostic = [nelson_exe, arguments, newline, output];
asserts.isequal(status, 7, diagnostic);
asserts.istrue(isfile(probe_file), diagnostic);
rmfile(probe_file);
%=============================================================================
% exit requested from a figure callback, run between two prompts
probe_file = [tempname(), '.txt'];
nelson_command = ['f = figure(''DeleteFcn'', ''exit(9)''); plot(1:10); drawnow(); ', ...
  'fid = fopen(''', replace(probe_file, '\', '/'), ''', ''wt''); ', ...
  'fprintf(fid, ''alive''); fclose(fid); close(f)'];
arguments = [' --quiet --no-ipc --no-user-startup --execute="', nelson_command, '"'];
[status, output] = system([quoted_exe, arguments], 180);
diagnostic = [nelson_exe, arguments, newline, output];
asserts.isequal(status, 9, diagnostic);
asserts.istrue(isfile(probe_file), diagnostic);
rmfile(probe_file);
%=============================================================================
