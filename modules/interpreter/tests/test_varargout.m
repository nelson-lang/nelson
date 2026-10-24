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
asserts.checkerror('a = nolhs_function();', _('Too many output arguments.'));
asserts.checkerror('disp(nolhs_function())', _('Too many output arguments.'));
onelhs_function()
asserts.istrue(onelhs_function());
twolhs_function()
a = twolhs_function();
asserts.isequal(a, 1);
[a, b] = twolhs_function();
asserts.isequal(a, 1);
asserts.isequal(b, 10);
varargoutlhs_function(0)
varargoutlhs_function(1)
asserts.checkerror('A = varargoutlhs_function(0)', _('Not enough outputs in varargout to satisfy call.'));
A = varargoutlhs_function(1)
asserts.isequal(A, 1);
A = varargoutlhs_function(2);
asserts.isequal(A, 1);
[A, B] = varargoutlhs_function(2);
asserts.isequal(A, 1);
asserts.isequal(B, 2);
displayOrder = evalc('[firstOutput, secondOutput] = max([1 3 2])');
firstPosition = strfind(displayOrder, 'firstOutput =');
secondPosition = strfind(displayOrder, 'secondOutput =');
asserts.istrue(~isempty(firstPosition));
asserts.istrue(~isempty(secondPosition));
asserts.istrue(firstPosition(1) < secondPosition(1));
%=============================================================================
