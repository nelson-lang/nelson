%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table([1; NaN; 3], {'a'; ''; 'c'}, 'VariableNames', {'A', 'B'});
M = ismissing(T);
assert_isequal(M, [false false; true true; false false]);
R = rmmissing(T);
assert_isequal(height(R), 2);
assert_checkerror('fillmissing(T, ''constant'', 0)', _('Invalid fill constant type for table variable.'));
F = fillmissing(T, 'constant', 0, 'DataVariables', 'A');
assert_isequal(F.A, [1; 0; 3]);
assert_isequal(F.B, {'a'; ''; 'c'});
S = standardizeMissing(table([1; -99; 3], 'VariableNames', {'A'}), -99);
assert_istrue(isnan(S.A(2)));
%=============================================================================
