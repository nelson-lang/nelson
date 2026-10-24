%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% nelson-gui and nelson-webview are windowed binaries without a console: they
% must run --execute and honour exit(code) exactly like nelson-adv-cli, and
% never stay alive waiting for a startup dialog nobody can close.
%=============================================================================
if ~ispc()
  return
end
%=============================================================================
binpath = modulepath('nelson', 'bin');
executables = {[binpath, '/nelson-gui'], [binpath, '/nelson-webview']};
%=============================================================================
for k = 1:numel(executables)
  nelson_exe = executables{k};
  if ~isfile([nelson_exe, '.exe'])
    continue
  end
  quoted_exe = ['"', replace(nelson_exe, '"', '\"'), '"'];

  % a command given inline (--execute=...) and detached (--execute ...)
  for use_inline_value = [1 0]
    probe_file = [tempname(), '.txt'];
    nelson_command = ['fid = fopen(''', replace(probe_file, '\', '/'), ''', ''wt''); ', ...
      'fprintf(fid, ''alive''); fclose(fid); exit(42)'];
    if use_inline_value
      arguments = [' --quiet --no-ipc --no-user-startup --execute="', nelson_command, '"'];
    else
      arguments = [' --quiet --no-ipc --no-user-startup --execute "', nelson_command, '"'];
    end
    [status, output] = system([quoted_exe, arguments], 180);
    diagnostic = [nelson_exe, arguments, newline, output];
    asserts.isequal(status, 42, diagnostic);
    asserts.istrue(isfile(probe_file), diagnostic);
    asserts.isequal(fileread(probe_file), 'alive');
    rmfile(probe_file);
  end

  % a script given to run(), as the test suites drive desktop mode
  probe_file = [tempname(), '.txt'];
  script_file = [tempname(), '.m'];
  filewrite(script_file, {['fid = fopen(''', replace(probe_file, '\', '/'), ''', ''wt'');'], ...
    'fprintf(fid, ''alive'');', 'fclose(fid);', 'exit(42);'});
  arguments = [' --quiet --no-ipc --no-user-startup --execute="run(''', ...
    replace(script_file, '\', '/'), ''')"'];
  [status, output] = system([quoted_exe, arguments], 180);
  diagnostic = [nelson_exe, arguments, newline, output];
  asserts.isequal(status, 42, diagnostic);
  asserts.istrue(isfile(probe_file), diagnostic);
  asserts.isequal(fileread(probe_file), 'alive');
  rmfile(probe_file);
  rmfile(script_file);
end
%=============================================================================
