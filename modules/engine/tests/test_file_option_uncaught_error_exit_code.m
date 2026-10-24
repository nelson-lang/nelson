%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
% An error left uncaught by the -f script ends the session with exit code 1;
% a script that completes (or catches its errors) still exits with 0, and an
% explicit exit(code) keeps its own code.
binpath = modulepath('nelson', 'bin');
nelson_cli = [binpath, '/nelson-cli'];
test_directory = tempname();
mkdir(test_directory);
cleanup_directory = onCleanup(@() rmdir(test_directory, 's'));
%=============================================================================
[status, output] = run_script(nelson_cli, test_directory, 'script_error.m', 'error(''my:id'', ''boom'');');
asserts.isequal(status, 1, output);
asserts.istrue(contains(output, 'boom'));
%=============================================================================
[status, output] = run_script(nelson_cli, test_directory, 'script_assert.m', ['asserts.isequal(1, 2);', newline, 'quit']);
asserts.isequal(status, 1, output);
%=============================================================================
[status, output] = run_script(nelson_cli, test_directory, 'script_parse_error.m', 'x = [1 2');
asserts.isequal(status, 1, output);
%=============================================================================
[status, output] = run_script(nelson_cli, test_directory, 'script_ok.m', 'disp(''done'');');
asserts.isequal(status, 0, output);
asserts.istrue(contains(output, 'done'));
%=============================================================================
[status, output] = run_script(nelson_cli, test_directory, 'script_caught.m', 'try, error(''boom''); catch, end');
asserts.isequal(status, 0, output);
%=============================================================================
[status, output] = run_script(nelson_cli, test_directory, 'script_comment.m', '% nothing to run');
asserts.isequal(status, 0, output);
%=============================================================================
[status, output] = run_script(nelson_cli, test_directory, 'script_exit.m', 'exit(3);');
asserts.isequal(status, 3, output);
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, [' --quiet --no-user-startup --no-ipc -f "', test_directory, '/missing_script.m"']);
asserts.isequal(status, 1, output);
%=============================================================================
function [status, output] = run_script(nelson_cli, directory, name, content)
  filename = [directory, '/', name];
  filewrite(filename, content);
  [status, output] = run_nelson_cli(nelson_cli, [' --quiet --no-user-startup --no-ipc -f "', filename, '"']);
end
%=============================================================================
