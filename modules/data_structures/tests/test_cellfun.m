%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('cellfun'), -1);
assert_isequal(nargout('cellfun'), -1);
%=============================================================================
C = {1:10, eye(3,4), eye(5,6)};
f = str2func('size');
[nrows, ncols] = cellfun(f, C,'UniformOutput', false);
assert_isequal(nrows, {1, 3, 5});
assert_isequal(ncols, {10, 4, 6});
%=============================================================================
C = {1:10, eye(3,4), eye(5,6)};
f = str2func('size');
[nrows, ncols] = cellfun(f, C,'UniformOutput', true);
assert_isequal(nrows, [1, 3, 5]);
assert_isequal(ncols, [10, 4, 6]);
%=============================================================================
a = cellfun (str2func('atan'), {1, 0});
assert_isapprox(a, [0.78540   0.00000], 1e-4);
%=============================================================================
a = cellfun ('isempty', {ones(3, 1, 3), '0'});
assert_isequal(a, [false false]);
%=============================================================================
a = cellfun ('islogical', {ones(3, 1, 3), logical(1)});
assert_isequal(a, [false true]);
%=============================================================================
a = cellfun ('isreal', {ones(3,1), logical(1)});
assert_isequal(a, [true true]);
%=============================================================================
a = cellfun ('length', {ones(3,2), logical(1)});
assert_isequal(a, [3 1]);
%=============================================================================
a = cellfun ('prodofsize', {ones(3,2,4), logical(1)});
assert_isequal(a, [24 1]);
%=============================================================================
a = cellfun ('isclass', {ones(3,2,4), logical(1)} , 'double');
assert_isequal(a, [true false]);
%=============================================================================
a = cellfun ('prodofsize', {[3 3], '0'});
assert_isequal(a, [2 1]);
%=============================================================================
a = cellfun ('ndims', {ones(3,1,3), '0'});
assert_isequal(a, [3 2]);
%=============================================================================
a = cellfun ('isclass', {'1', '0'}, 'char');
assert_isequal(a, [true true]);
%=============================================================================
greetings = {'Hello','Guten Tag','Sawadee','Bonjour','Namaste',''};
R1 = cellfun('size',greetings, 2);
R2 = cellfun('length', greetings);
assert_isequal(R1, R2);
assert_isequal(R1, [ 5     9     7     7     7     0]);
%=============================================================================
addpath(modulepath('data_structures', 'tests'));
R = str2func('fun1');
H =  str2func('errorfun');
A = {rand(3)};
B = {rand(5)};
AgtA = cellfun(R, A, B, 'ErrorHandler', H,  'UniformOutput', true);
assert_isequal(AgtA, false);
AgtB = cellfun(R, A, B, 'ErrorHandler', H,  'UniformOutput', false);
assert_isequal(AgtB, {false});
%=============================================================================
assert_istrue(all(cellfun('isnumeric', {})));
assert_istrue(all(cellfun('isnumeric', {}, 'UniformOutput', true)));
%=============================================================================
R = cellfun('isnumeric', {}, 'UniformOutput', false)
assert_isequal(R, {});
%=============================================================================
inputArguments = 'Faces';
assert_checkerror('cellfun(''isclass'', inputArguments, ''char'')', _('cellfun works only on cells.'));
%=============================================================================