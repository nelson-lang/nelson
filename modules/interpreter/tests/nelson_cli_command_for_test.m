%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [nelson_exe, execute_option] = nelson_cli_command_for_test()
  binpath = modulepath('nelson', 'bin');
  cli = [binpath, '/nelson-cli'];
  if executable_exists(cli)
    nelson_exe = quote_command_path(cli);
    execute_option = ' -q -e ';
    return
  end
  portable_cli = [binpath, '/nelson-cli-exec'];
  if executable_exists(portable_cli)
    nelson_exe = quote_command_path(portable_cli);
    execute_option = ' --quiet --execute ';
    return
  end
  skip_testsuite('Nelson command-line executable is not available.');
end
%=============================================================================
function tf = executable_exists(filename)
  % On Windows the executable carries a '.exe' suffix that the command line
  % resolves implicitly, so probe both spellings before deciding to skip.
  tf = isfile(filename) || (ispc() && isfile([filename, '.exe']));
end
%=============================================================================
function quoted = quote_command_path(filename)
  quoted = ['"', filename, '"'];
end
%=============================================================================
