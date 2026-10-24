%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('crosstab'), -1);
asserts.isequal(nargout('crosstab'), -1);
%=============================================================================
[tbl, chi2, p, labels] = crosstab([1 1 1 2 2 2], {'a', 'a', 'b', 'b', 'b', 'b'});
asserts.isequal(tbl, [2 1; 0 3]);
asserts.isequal(chi2, 3);
asserts.isapprox(p, 0.083264516663550, 1e-14);
asserts.isequal(labels, {'1', 'a'; '2', 'b'});
%=============================================================================
tbl = crosstab([1 1 2 2], {'a', 'b', 'a', 'b'}, ["x", "x", "y", "y"]);
asserts.isequal(tbl(:, :, 1), [1 1; 0 0]);
asserts.isequal(tbl(:, :, 2), [0 0; 1 1]);
%=============================================================================
[tbl, ~, ~, labels] = crosstab([1 NaN 2], {'a', 'a', 'b'});
asserts.isequal(tbl, [1 0; 0 1]);
asserts.isequal(labels, {'1', 'a'; '2', 'b'});
%=============================================================================
[tbl, ~, ~, labels] = crosstab([1 NaN 2], {'a', 'a', 'b'}, 'IncludeMissingGroups', true);
asserts.isequal(tbl, [1 0; 0 1; 1 0]);
asserts.isequal(labels(1:3, 1), {'1'; '2'; 'NaN'});
%=============================================================================
T = table([1; 1; 2], {'a'; 'b'; 'a'}, 'VariableNames', {'G1', 'G2'});
[tbl, ~, ~, labels] = crosstab(T);
asserts.isequal(tbl, [1 1; 1 0]);
asserts.isequal(labels, {'1', 'a'; '2', 'b'});
%=============================================================================
S = crosstab([1 1 2], {'a', 'b', 'a'}, 'OutputFormat', 'stacked-table');
asserts.istrue(istable(S));
asserts.isequal(S.Count, [1; 1; 1; 0]);
%=============================================================================
[tblStable, ~, ~, labelsStable] = crosstab({'Male'; 'Male'; 'Female'}, [1; 2; 1]);
asserts.isequal(tblStable, [1 1; 1 0]);
asserts.isequal(labelsStable(1:2, 1), {'Male'; 'Female'});
%=============================================================================
asserts.checkerror('crosstab()', _('Not enough input arguments.'));
asserts.checkerror('crosstab([1 2], [1 2 3])', _('All grouping variables must have the same number of elements.'));
asserts.checkerror('crosstab({1, 2}, [1 2])', _('Grouping variables must be vectors or text arrays.'));
asserts.checkerror('crosstab([1 2], [1 2], ''OutputFormat'', ''bad'')', sprintf(_('Unsupported %s.'), 'output format'));
%=============================================================================
