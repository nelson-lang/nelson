%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([nelsonroot(), '/modules/interpreter/tests/']);
nolhs_function()
assert_checkerror('a = nolhs_function();', _('Wrong number of output arguments.'));
assert_checkerror('disp(nolhs_function())', _('Wrong number of output arguments.'));
onelhs_function()
assert_istrue(onelhs_function());
twolhs_function()
a = twolhs_function();
assert_isequal(a, 1);
[a, b] = twolhs_function();
assert_isequal(a, 1);
assert_isequal(b, 10);
varargoutlhs_function(0)
varargoutlhs_function(1)
assert_checkerror('A = varargoutlhs_function(0)', _('Not enough outputs in varargout to satisfy call.'));
A = varargoutlhs_function(1)
assert_isequal(A, 1);
A = varargoutlhs_function(2);
assert_isequal(A, 1);
[A, B] = varargoutlhs_function(2);
assert_isequal(A, 1);
assert_isequal(B, 2);
%=============================================================================
