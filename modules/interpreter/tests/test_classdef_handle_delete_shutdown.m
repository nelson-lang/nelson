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
% Handle objects still held by the base workspace when the interpreter
% shuts down run their delete method, in workspace order, before exiting.
skip_testsuite((strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()), ...
  'Portable runtime classdef handle shutdown cleanup is not aligned yet.');
[nelson_exe, execute_option] = nelson_cli_command_for_test();
%=============================================================================
code = ['path_test=[modulepath(''interpreter'',''tests''), ''/classdef'']; ', ...
        'addpath(path_test); ', ...
        'base1 = ClassdefLifecycleEcho(''base1''); ', ...
        'base2 = ClassdefLifecycleEcho(''base2''); ', ...
        'disp(''END OF SCRIPT''); ', ...
        'quit'];
cmd = [nelson_exe, execute_option, '"', code, '"'];
[status, msg] = system(cmd, 60);
asserts.isequal(status, 0);
lines = strtrim(strsplit(strtrim(msg), newline));
asserts.isequal(lines, {'END OF SCRIPT', 'DELETE base1', 'DELETE base2'});
%=============================================================================
% clear all also runs the destructors, before the next statement
code = ['path_test=[modulepath(''interpreter'',''tests''), ''/classdef'']; ', ...
        'addpath(path_test); ', ...
        'p = ClassdefLifecycleEcho(''p1''); ', ...
        'q = ClassdefLifecycleEcho(''q1''); ', ...
        'clear all; ', ...
        'disp(''after clear all''); ', ...
        'quit'];
cmd = [nelson_exe, execute_option, '"', code, '"'];
[status, msg] = system(cmd, 60);
asserts.isequal(status, 0);
lines = strtrim(strsplit(strtrim(msg), newline));
asserts.isequal(lines, {'DELETE p1', 'DELETE q1', 'after clear all'});
%=============================================================================
