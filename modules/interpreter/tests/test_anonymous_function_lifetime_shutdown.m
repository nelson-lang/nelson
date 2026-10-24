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
% A handle object captured by an anonymous function still held by the base
% workspace at shutdown runs its delete method before the process exits.
skip_testsuite((strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()), ...
  'Portable runtime classdef handle shutdown cleanup is not aligned yet.');
[nelson_exe, execute_option] = nelson_cli_command_for_test();
%=============================================================================
code = ['path_test=[modulepath(''interpreter'',''tests''), ''/classdef'']; ', ...
        'addpath(path_test); ', ...
        'obj = ClassdefLifecycleEcho(''captured''); ', ...
        'f = @() obj.Name; ', ...
        'clear obj; ', ...
        'disp(''END OF SCRIPT''); ', ...
        'quit'];
cmd = [nelson_exe, execute_option, '"', code, '"'];
[status, msg] = system(cmd, 60);
asserts.isequal(status, 0);
lines = strtrim(strsplit(strtrim(msg), newline));
asserts.isequal(lines, {'END OF SCRIPT', 'DELETE captured'});
%=============================================================================
