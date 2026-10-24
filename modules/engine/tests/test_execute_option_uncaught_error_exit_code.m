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
% An error left uncaught by the -e command ends the session with exit code 1;
% a command that completes (or catches its errors) still exits with 0, and an
% explicit exit(code) keeps its own code.
binpath = modulepath('nelson', 'bin');
nelson_cli = [binpath, '/nelson-cli'];
options = ' --quiet --no-user-startup --no-ipc -e ';
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, [options, '"error(''my:id'', ''boom'')"']);
asserts.isequal(status, 1, output);
asserts.istrue(contains(output, 'boom'));
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, [options, '"asserts.isequal(1, 2); quit"']);
asserts.isequal(status, 1, output);
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, [options, '"undefined_function_for_exit_code_test"']);
asserts.isequal(status, 1, output);
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, [options, '"x = [1 2"']);
asserts.isequal(status, 1, output);
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, [options, '"disp(''done'')"']);
asserts.isequal(status, 0, output);
asserts.istrue(contains(output, 'done'));
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, [options, '"try, error(''boom''); catch, end"']);
asserts.isequal(status, 0, output);
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, [options, '"% nothing to run"']);
asserts.isequal(status, 0, output);
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, [options, '"exit(3)"']);
asserts.isequal(status, 3, output);
%=============================================================================
