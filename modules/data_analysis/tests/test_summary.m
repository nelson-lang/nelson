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
asserts.isequal(fieldnames(S), {'A'; 'B'});
asserts.isequal(S.A.Type, 'double');
asserts.isequal(S.A.Size, [3 1]);
asserts.isequal(S.A.Description, 'numeric variable');
asserts.isequal(S.A.Units, 'm');
asserts.isequal(S.A.NumMissing, 1);
asserts.isequal(S.A.Min, 1);
asserts.isequal(S.A.Median, 2);
asserts.isequal(S.A.Max, 3);
asserts.isequal(S.A.Mean, 2);
asserts.isapprox(S.A.Std, sqrt(2), 1e-12);
asserts.isequal(S.B.Type, 'logical');
asserts.isequal(S.B.True, 2);
asserts.isequal(S.B.False, 1);
%=============================================================================
A = categorical({'red', 'blue', 'red', ''});
out = evalc('summary(A);');
asserts.istrue(contains(out, 'red: 2'));
asserts.istrue(contains(out, 'blue: 1'));
asserts.istrue(contains(out, '<undefined>: 1'));
%=============================================================================
% summary of a categorical with an output returns a struct
S = summary(A);
asserts.istrue(isstruct(S));
asserts.isequal(S.Size, [1 4]);
asserts.isequal(S.Type, 'categorical');
asserts.isequal(sort(S.Categories), {'blue'; 'red'});
asserts.isequal(S.NumMissing, 1);
%=============================================================================
% Counts follows the orientation of the input
B = categorical({'a', 'b', 'a', 'c', 'a'});
SB = summary(B);
asserts.isequal(size(SB.Counts), [1 3]);
asserts.isequal(SB.Counts, [3 1 1]);
C = categorical({'a'; 'b'; 'a'});
SC = summary(C);
asserts.isequal(size(SC.Counts), [2 1]);
asserts.isequal(SC.Counts, [2; 1]);
%=============================================================================
asserts.checkerror('summary(1)', _('Input must be a table or categorical array.'));
%=============================================================================
