%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table([1; NaN; 3], logical([true; false; true]), 'VariableNames', {'A', 'B'});
T.Properties.VariableDescriptions = {'numeric variable', 'logical variable'};
T.Properties.VariableUnits = {'m', ''};
S = summary(T);
assert_isequal(fieldnames(S), {'A'; 'B'});
assert_isequal(S.A.Type, 'double');
assert_isequal(S.A.Size, [3 1]);
assert_isequal(S.A.Description, 'numeric variable');
assert_isequal(S.A.Units, 'm');
assert_isequal(S.A.NumMissing, 1);
assert_isequal(S.A.Min, 1);
assert_isequal(S.A.Median, 2);
assert_isequal(S.A.Max, 3);
assert_isequal(S.A.Mean, 2);
assert_isapprox(S.A.Std, sqrt(2), 1e-12);
assert_isequal(S.B.Type, 'logical');
assert_isequal(S.B.True, 2);
assert_isequal(S.B.False, 1);
%=============================================================================
