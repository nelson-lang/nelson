%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('cellfun'), -1);
asserts.isequal(nargout('cellfun'), -1);
%=============================================================================
C = {1:10, eye(3, 4), eye(5, 6)};
f = str2func('size');
[nrows, ncols] = cellfun(f, C, 'UniformOutput', false);
asserts.isequal(nrows, {1, 3, 5});
asserts.isequal(ncols, {10, 4, 6});
%=============================================================================
C = {1:10, eye(3, 4), eye(5, 6)};
f = str2func('size');
[nrows, ncols] = cellfun(f, C, 'UniformOutput', true);
asserts.isequal(nrows, [1, 3, 5]);
asserts.isequal(ncols, [10, 4, 6]);
%=============================================================================
a = cellfun(str2func('atan'), {1, 0});
asserts.isapprox(a, [0.78540 0.00000], 1e-4);
%=============================================================================
a = cellfun('isempty', {ones(3, 1, 3), '0'});
asserts.isequal(a, [false false]);
%=============================================================================
a = cellfun('islogical', {ones(3, 1, 3), logical(1)});
asserts.isequal(a, [false true]);
%=============================================================================
a = cellfun('isreal', {ones(3, 1), logical(1)});
asserts.isequal(a, [true true]);
%=============================================================================
a = cellfun('length', {ones(3, 2), logical(1)});
asserts.isequal(a, [3 1]);
%=============================================================================
a = cellfun('prodofsize', {ones(3, 2, 4), logical(1)});
asserts.isequal(a, [24 1]);
%=============================================================================
a = cellfun('isclass', {ones(3, 2, 4), logical(1)}, 'double');
asserts.isequal(a, [true false]);
%=============================================================================
a = cellfun('prodofsize', {[3 3], '0'});
asserts.isequal(a, [2 1]);
%=============================================================================
a = cellfun('ndims', {ones(3, 1, 3), '0'});
asserts.isequal(a, [3 2]);
%=============================================================================
a = cellfun('isclass', {'1', '0'}, 'char');
asserts.isequal(a, [true true]);
%=============================================================================
greetings = {'Hello', 'Guten Tag', 'Sawadee', 'Bonjour', 'Namaste', ''};
R1 = cellfun('size', greetings, 2);
R2 = cellfun('length', greetings);
asserts.isequal(R1, R2);
asserts.isequal(R1, [5 9 7 7 7 0]);
%=============================================================================
addpath(modulepath('data_structures', 'tests'));
R = str2func('fun1');
H = str2func('errorfun');
A = {rand(3)};
B = {rand(5)};
AgtA = cellfun(R, A, B, 'ErrorHandler', H, 'UniformOutput', true);
asserts.isequal(AgtA, false);
AgtB = cellfun(R, A, B, 'ErrorHandler', H, 'UniformOutput', false);
asserts.isequal(AgtB, {false});
%=============================================================================
asserts.istrue(all(cellfun('isnumeric', {})));
asserts.istrue(all(cellfun('isnumeric', {}, 'UniformOutput', true)));
%=============================================================================
R = cellfun('isnumeric', {}, 'UniformOutput', false)
asserts.isequal(R, {});
%=============================================================================
inputArguments = 'Faces';
asserts.checkerror('cellfun(''isclass'', inputArguments, ''char'')', _('cellfun works only on cells.'));
%=============================================================================
CellInput = {1:10, eye(3, 4)};
[Rows, Columns] = cellfun(str2func('size'), CellInput, 'UniformOutput', true);
asserts.isequal(Rows, [1 3]);
asserts.isequal(Columns, [10 4]);
asserts.isequal(cellfun('isempty', {ones(3, 1, 3), '0'}), [false false]);
asserts.isequal(cellfun('isclass', {'1', '0'}, 'char'), [true true]);
asserts.isequal(cellfun('length', {ones(3, 2), logical(1)}), [3 1]);
asserts.isequal(cellfun('prodofsize', {ones(3, 2, 4), logical(1)}), [24 1]);
asserts.isequal(cellfun('ndims', {ones(3, 1, 3), '0'}), [3 2]);
asserts.istrue(all(cellfun('isnumeric', {})));
asserts.isequal(cellfun('isnumeric', {}, 'UniformOutput', false), {});
asserts.checkerror('cellfun(''isclass'', ''abc'', ''char'')', _('cellfun works only on cells.'));
%=============================================================================
% uniform collection of scalar string outputs
r = cellfun(@(x) string(x), {'a', 'bb', 'ccc'});
assert_isequal(r, ["a" "bb" "ccc"]);
r = cellfun(@(x) string(numel(x)), {[1 2], [3 4 5]});
assert_isequal(r, ["2" "3"]);
%=============================================================================

% 'UniformOutput' accepts case-insensitive prefix abbreviations (un, unif, ...)
r = cellfun(@(x) x * 2, {1, 2, 3}, 'un', false);
assert_isequal(r, {2, 4, 6});
r = cellfun(@(x) x * 2, {1, 2}, 'Un', 0);
assert_isequal(r, {2, 4});
r = cellfun(@(x)[x, x], {1, 2}, 'uniform', false);
assert_isequal(r, {[1 1], [2 2]});
r = cellfun(@(x)[x x], {1, 2}, 'ErrorHandler', @(s, x) 0, 'un', false);
assert_isequal(r, {[1 1], [2 2]});
%=============================================================================
% the error handler receives the element index; a string names the function
R = cellfun(@(x) str2num(x), {1 2}, 'ErrorHandler', @(e, x) e.index);
asserts.isequal(R, [1 2]);
R = cellfun(@(x) str2num(x), {1}, 'ErrorHandler', @(e, x) fieldnames(e), 'UniformOutput', false);
asserts.isequal(R, {{'identifier'; 'message'; 'index'}});
R = cellfun("isempty", {1, []});
asserts.isequal(R, [false true]);
%=============================================================================
