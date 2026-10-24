%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% numel(T) is height(T) * width(T); indexing still yields one value.
%=============================================================================
T = table([1; 2], [3; 4]);
asserts.isequal(numel(T), 4);
asserts.isequal(numel(table()), 0);
asserts.isequal(numel(table.empty(0, 3)), 0);
asserts.isequal(numel(T(:, [])), 0);
asserts.isequal(numel(table([1; 2; 3], [4; 5; 6], [7; 8; 9])), 9);
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2]);
asserts.isequal(numel(TT), 2);
E = eventtable(seconds([1; 2; 3]), 'EventLabels', ["a"; "b"; "c"]);
asserts.isequal(numel(E), 3);
%=============================================================================
% numel(T, rows, vars): count of selected elements, no range check
asserts.isequal(numel(T, 1, 1), 1);
asserts.isequal(numel(T, ':', 1), 2);
asserts.isequal(numel(T, 1:2, ':'), 4);
asserts.isequal(numel(T, 5, 1), 1);
asserts.isequal(numel(T, 1, "Var2"), 1);
asserts.isequal(numel(T, 1, {'Var1', 'Var2'}), 2);
asserts.isequal(numel(T, logical([1 0]), ':'), 2);
asserts.isequal(numel(T, [1 1 2], ':'), 6);
asserts.isequal(numel(TT, ':', 1), 2);
msg = 'Parentheses indexing requires row and variable subscripts.';
assert_checkerror('numel(T, [1 2])', msg, 'Nelson:table:invalidSubscript:valueIndexingRequiresRowVariableSubscripts');
assert_checkerror('numel(T, 1, 1, 1)', msg, 'Nelson:table:invalidSubscript:valueIndexingRequiresRowVariableSubscripts');
%=============================================================================
% indexing still produces exactly one output
c = {T.Var1};
asserts.isequal(c, {[1; 2]});
c = {T{:, 1}};
asserts.isequal(c, {[1; 2]});
c = {T(1, :)};
asserts.isequal(numel(c), 1);
c = {TT.Var1};
asserts.isequal(c, {[1; 2]});
c = {T.Properties.VariableNames};
asserts.isequal(c, {{'Var1', 'Var2'}});
asserts.isequal(T{end, end}, 4);
asserts.isequal(TT{end, 1}, 2);
asserts.isequal(T.Var1(end), 2);
%=============================================================================
asserts.isequal(ndims(T), 2);
asserts.istrue(isempty(table()));
asserts.isfalse(isempty(T));
asserts.istrue(isempty(T(:, [])));
msg = 'Undefined function ''length'' for input arguments of type ''table''. Use the height, width, or size functions instead.';
assert_checkerror('length(T)', msg, 'Nelson:table:UndefinedLengthFunction');
msg = 'Undefined function ''length'' for input arguments of type ''timetable''. Use the height, width, or size functions instead.';
assert_checkerror('length(TT)', msg, 'Nelson:table:UndefinedLengthFunction');
%=============================================================================
