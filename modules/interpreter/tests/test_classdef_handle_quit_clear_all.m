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
[nelson_exe, execute_option] = nelson_cli_command_for_test();
%=============================================================================
code = ['path_test=[modulepath(''interpreter'',''tests''), ''/classdef'']; ', ...
        'addpath(path_test); ', ...
        'h=ClassdefParenReferenceHandle([10, 20]); ', ...
        'disp(''constructed''); ', ...
        'clear all; ', ...
        'disp(''cleared''); ', ...
        'quit'];
cmd = [nelson_exe, execute_option, '"', code, '"'];
[status, msg] = system(cmd, 60);
asserts.isequal(status, 0);
asserts.istrue(contains(msg, 'constructed'));
asserts.istrue(contains(msg, 'cleared'));
%=============================================================================
