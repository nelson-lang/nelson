%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% ismember/intersect/setdiff/union/setxor on tables
%=============================================================================
% variable names must match
A = table([1; 2]);
B = table([1; 3], 'VariableNames', {'X'});
msg = _('Table variable names must match.');
id = 'Nelson:table:variableNamesMustMatch';
asserts.checkerror('ismember(A, B)', msg, id);
asserts.checkerror('intersect(A, B)', msg, id);
asserts.checkerror('setdiff(A, B)', msg, id);
asserts.checkerror('union(A, B)', msg, id);
asserts.checkerror('setxor(A, B)', msg, id);
asserts.checkerror('union(A, table([1; 3], [2; 4]))', msg, id);
%=============================================================================
% the second input must be a table
msg = _('Second input must be a table.');
asserts.checkerror('union(A, 5)', msg, 'Nelson:table:invalidType');
asserts.checkerror('ismember(A, [1; 2])', msg, 'Nelson:table:invalidType');
%=============================================================================
% the variable order may differ: B is compared by name, C follows A
A = table([3; 1; NaN; 1], {'c'; 'a'; 'n'; 'a'}, 'VariableNames', {'x', 'y'});
B = table({'a'; 'z'; 'n'; 'a'}, [1; 5; NaN; 1], 'VariableNames', {'y', 'x'});
%=============================================================================
[tf, loc] = ismember(A, B);
asserts.isequal(tf, [false; true; false; true]);
asserts.isequal(loc, [0; 1; 0; 1]);
[tf2, loc2] = ismember(A, B, 'rows');
asserts.isequal(tf2, tf);
asserts.isequal(loc2, loc);
%=============================================================================
% sorted by default (NaN rows are distinct and come last)
[C, ia, ib] = union(A, B);
asserts.isequal(C.Properties.VariableNames, {'x', 'y'});
asserts.isequal(C.x, [1; 3; 5; NaN; NaN]);
asserts.isequal(C.y, {'a'; 'c'; 'z'; 'n'; 'n'});
asserts.isequal(ia, [2; 1; 3]);
asserts.isequal(ib, [2; 3]);
%=============================================================================
[C, ia, ib] = union(A, B, 'stable');
asserts.isequal(C.x, [3; 1; NaN; 5; NaN]);
asserts.isequal(ia, [1; 2; 3]);
asserts.isequal(ib, [2; 3]);
%=============================================================================
[C, ia, ib] = intersect(A, B);
asserts.isequal(C.x, 1);
asserts.isequal(C.y, {'a'});
asserts.isequal(ia, 2);
asserts.isequal(ib, 1);
%=============================================================================
[C, ia] = setdiff(A, B);
asserts.isequal(C.x, [3; NaN]);
asserts.isequal(ia, [1; 3]);
%=============================================================================
[C, ia, ib] = setxor(A, B);
asserts.isequal(C.x, [3; 5; NaN; NaN]);
asserts.isequal(C.y, {'c'; 'z'; 'n'; 'n'});
asserts.isequal(ia, [1; 3]);
asserts.isequal(ib, [2; 3]);
[C, ia, ib] = setxor(A, B, 'stable');
asserts.isequal(C.x, [3; NaN; 5; NaN]);
asserts.isequal(ia, [1; 3]);
asserts.isequal(ib, [2; 3]);
%=============================================================================
% sorted order follows sortrows across the variables
A = table([2; 1; 2; 1], {'b'; 'z'; 'a'; 'y'}, 'VariableNames', {'K', 'S'});
B = table([1; 3], {'z'; 'a'}, 'VariableNames', {'K', 'S'});
[C, ia, ib] = union(A, B);
asserts.isequal(C.K, [1; 1; 2; 2; 3]);
asserts.isequal(C.S, {'y'; 'z'; 'a'; 'b'; 'a'});
asserts.isequal(ia, [4; 2; 3; 1]);
asserts.isequal(ib, 2);
[C, ia] = setdiff(A, B);
asserts.isequal(C.S, {'y'; 'a'; 'b'});
asserts.isequal(ia, [4; 3; 1]);
[C, ia] = setdiff(A, B, 'stable');
asserts.isequal(C.S, {'b'; 'a'; 'y'});
asserts.isequal(ia, [1; 3; 4]);
%=============================================================================
% row names are ignored in the comparison, C keeps the row names of A
A = table([1; 2; 3], 'VariableNames', {'V'}, 'RowNames', {'Li'; 'Wu'; 'Ko'});
B = table([2; 5], 'VariableNames', {'V'}, 'RowNames', {'Smith'; 'Li'});
[tf, loc] = ismember(A, B);
asserts.isequal(tf, [false; true; false]);
asserts.isequal(loc, [0; 1; 0]);
[C, ia, ib] = intersect(A, B);
asserts.isequal(C.Properties.RowNames, {'Wu'});
asserts.isequal(ia, 2);
asserts.isequal(ib, 1);
[C, ia] = setdiff(A, B);
asserts.isequal(C.Properties.RowNames, {'Li'; 'Ko'});
asserts.isequal(ia, [1; 3]);
%=============================================================================
% string and categorical variables
A = table(["b"; "a"; "b"], categorical({'x'; 'y'; 'x'}), 'VariableNames', {'S', 'G'});
B = table(["a"; "c"], categorical({'y'; 'x'}), 'VariableNames', {'S', 'G'});
[C, ia, ib] = union(A, B);
asserts.isequal(C.S, ["a"; "b"; "c"]);
asserts.isequal(ia, [2; 1]);
asserts.isequal(ib, 2);
[tf, loc] = ismember(A, B);
asserts.isequal(tf, [false; true; false]);
asserts.isequal(loc, [0; 1; 0]);
%=============================================================================
% empty inputs give column index vectors
A = table([2; 1], 'VariableNames', {'K'});
E = A([], :);
[C, ia, ib] = union(A, E);
asserts.isequal(C.K, [1; 2]);
asserts.isequal(ia, [2; 1]);
asserts.isequal(size(ib), [0 1]);
[C, ia, ib] = intersect(A, E);
asserts.isequal(height(C), 0);
asserts.isequal(size(ia), [0 1]);
asserts.isequal(size(ib), [0 1]);
[tf, loc] = ismember(E, A);
asserts.isequal(size(tf), [0 1]);
asserts.isequal(size(loc), [0 1]);
%=============================================================================
% options are validated
msg = _('Unsupported option.');
A = table([1; 2]);
asserts.checkerror('union(A, A, ''legacy'')', msg, 'Nelson:data_analysis:unsupportedOption');
asserts.checkerror('intersect(A, A, ''foo'')', msg, 'Nelson:data_analysis:unsupportedOption');
%=============================================================================
