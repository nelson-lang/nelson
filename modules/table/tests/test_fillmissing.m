%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table([1; NaN; 3], 'VariableNames', {'A'});
R = fillmissing(T, 'constant', 0);
assert_isequal(R.A, [1; 0; 3]);
%=============================================================================
T = table([1; NaN; 3], {'a'; ''; 'c'}, 'VariableNames', {'A', 'B'});
assert_checkerror('fillmissing(T, ''constant'', 0)', _('Invalid fill constant type for table variable.'));
R = fillmissing(T, 'constant', 0, 'DataVariables', 'A');
assert_isequal(R.A, [1; 0; 3]);
assert_isequal(R.B, {'a'; ''; 'c'});
%=============================================================================
