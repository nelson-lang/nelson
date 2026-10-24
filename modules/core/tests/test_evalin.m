%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
addpath([nelsonroot(), '/modules/core/tests/']);
%=============================================================================
X = 33;
R = fun_evalin1();
asserts.isequal(R, X);
%=============================================================================
R = evalin('base', 'evalin(''caller'',''pi'')');
asserts.isequal(R, pi);
%=============================================================================
fun_evalin2();
R = acquirevar('base', 'XX');
asserts.isequal(R, 356);
%=============================================================================
assignin('base', 'compatibilityRuntimeValue', 42);
asserts.isequal(evalin('base', 'compatibilityRuntimeValue'), 42);
evalin('base', 'clear compatibilityRuntimeValue');
asserts.isequal(evalin('base', 'exist(''compatibilityRuntimeValue'', ''var'')'), 0);
%=============================================================================
restoreProbe = 17;
asserts.istrue(fun_evalin_error_restore());
asserts.isequal(restoreProbe, 17);
X = 18;
R = fun_evalin1();
asserts.isequal(R, X);
%=============================================================================
R = fun_evalin_nested_scope_driver();
asserts.isequal(R, 4);
%=============================================================================
