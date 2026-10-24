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
h = .5; x = 0:h:pi;
X = sin(x .^ 2);
R = diff(X);
REF = [0.2474 0.5941 -0.0634 -1.5349 0.7236 0.4453];
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
X = [1 3 5; 7 11 13; 17 19 23];
R = diff(X, 1, 2);
REF = [2 2; 4 2; 2 4];
asserts.isequal(R, REF);
%=============================================================================
V = [(1:8) .^ 2, (1:8) .^ 3, (1:8) .^ 4];
R = diff(V, 3, 2);
REF = [0, 0, 0, 0, 0, -80, 148, -58, 6, 6, 6, 6, 6, -722, 1206, -476, 60, 84, 108, 132, 156];
asserts.isequal(R, REF);
%=============================================================================
X = [1 3 5; 7 11 13; 17 19 23];
asserts.checkerror('R = diff(X, -1, 2);', sprintf(_('%s must be a positive integer scalar.'), 'Difference order N'));
asserts.checkerror('R = diff(X, 0, 2);', sprintf(_('%s must be a positive integer scalar.'), 'Difference order N'));
%=============================================================================
asserts.isequal(diff([1 4 9 16]), [3 5 7]);
%=============================================================================
% scalar / insufficient input returns empty (was: Index exceeds matrix dimensions)
asserts.isequal(diff(5), []);
%=============================================================================
asserts.isequal(diff([5]), []);
%=============================================================================
asserts.isequal(diff(single(5)), single([]));
%=============================================================================
% vectors: orientation is kept
R = diff([1 4 9 16]');
asserts.isequal(R, [3; 5; 7]);
%=============================================================================
R = diff([1 4 9 16 25], 2);
asserts.isequal(R, [2 2 2]);
%=============================================================================
R = diff([1 4 9 16], []);
asserts.isequal(R, [3 5 7]);
%=============================================================================
R = diff(1:4, 1, 2);
asserts.isequal(R, [1 1 1]);
%=============================================================================
R = diff((1:4)', 1, 1);
asserts.isequal(R, [1; 1; 1]);
%=============================================================================
R = diff(1:4, 1, 1);
asserts.isequal(size(R), [0 4]);
asserts.isequal(class(R), 'double');
%=============================================================================
R = diff((1:4)', 1, 2);
asserts.isequal(size(R), [4 0]);
%=============================================================================
R = diff([1 Inf NaN 2]);
asserts.istrue(isequaln(R, [Inf NaN NaN]));
%=============================================================================
% matrices: along the first non-singleton dimension, then the next ones
X = [1 3 5; 7 11 13; 17 19 23];
asserts.isequal(diff(X), [6 8 8; 10 8 10]);
asserts.isequal(diff(X, 2), [4 0 2]);
asserts.isequal(diff(X, 3), [-4 2]);
asserts.isequal(diff(X, 4), 6);
asserts.isequal(diff(X, 5), []);
asserts.isequal(diff(X, 1, 1), [6 8 8; 10 8 10]);
asserts.isequal(diff(X, 2, 2), [0; -2; 2]);
%=============================================================================
R = diff(X, 3, 1);
asserts.isequal(size(R), [0 3]);
asserts.isequal(class(R), 'double');
%=============================================================================
A = reshape(1:8, 2, 2, 2);
asserts.isequal(diff(A), ones(1, 2, 2));
asserts.isequal(diff(A, 1, 2), 2 * ones(2, 1, 2));
asserts.isequal(diff(A, 1, 3), 4 * ones(2, 2));
%=============================================================================
% empty inputs
R = diff([]);
asserts.isequal(size(R), [0 0]);
asserts.isequal(class(R), 'double');
%=============================================================================
asserts.isequal(size(diff(zeros(1, 0))), [0 0]);
asserts.isequal(size(diff(zeros(3, 0))), [2 0]);
asserts.isequal(size(diff(zeros(0, 3), 1, 2)), [0 2]);
%=============================================================================
R = diff(single([]));
asserts.isequal(size(R), [0 0]);
asserts.isequal(class(R), 'single');
%=============================================================================
R = diff(single(1:3), 1, 1);
asserts.isequal(size(R), [0 3]);
asserts.isequal(class(R), 'single');
%=============================================================================
% single
R = diff(single([1 4 9]));
asserts.isequal(R, single([3 5]));
%=============================================================================
R = diff(single([1 3 5; 7 11 13]), 1, 2);
asserts.isequal(R, single([2 2; 4 2]));
%=============================================================================
% complex: the result becomes real when every imaginary part cancels
R = diff([1 + 2i, 3 + 2i, 6 - 1i]);
asserts.isequal(R, [2, 3 - 3i]);
asserts.isfalse(isreal(R));
%=============================================================================
R = diff([1 + 1i, 3 + 1i]);
asserts.isequal(R, 2);
asserts.istrue(isreal(R));
%=============================================================================
R = diff(single([1 + 1i, 4 + 3i]));
asserts.isequal(R, single(3 + 2i));
%=============================================================================
% logical, char, integers (saturated) and sparse
R = diff([true false true]);
asserts.isequal(R, [-1 1]);
asserts.isequal(class(R), 'double');
%=============================================================================
R = diff('abd');
asserts.isequal(R, [1 2]);
asserts.isequal(class(R), 'double');
%=============================================================================
R = diff(int8([100 -100 50]));
asserts.isequal(R, int8([-128 127]));
%=============================================================================
R = diff(uint8([5 3 9]));
asserts.isequal(R, uint8([0 6]));
%=============================================================================
R = diff(int16([1 2; 4 8]), 1, 2);
asserts.isequal(R, int16([1; 4]));
%=============================================================================
R = diff(sparse([1 0 3]));
asserts.istrue(issparse(R));
asserts.isequal(full(R), [-1 3]);
%=============================================================================
R = diff(sparse([1 2; 3 5]));
asserts.istrue(issparse(R));
asserts.isequal(full(R), [2 3]);
%=============================================================================
% invalid arguments
msg = sprintf(_('%s must be a positive integer scalar.'), 'Difference order N');
asserts.checkerror('diff(1:3, 1.5)', msg, 'Nelson:diff:differenceOrderMustBePositiveInteger');
asserts.checkerror('diff(1:3, [1 2])', msg, 'Nelson:diff:differenceOrderMustBePositiveInteger');
asserts.checkerror('diff(1:3, NaN)', msg, 'Nelson:diff:differenceOrderMustBePositiveInteger');
%=============================================================================
msg = _('Dimension argument must be a positive integer scalar within indexing range.');
asserts.checkerror('diff(1:3, 1, 0)', msg, 'Nelson:getdimarg:dimensionMustBePositiveInteger');
asserts.checkerror('diff(1:3, 1, 1.5)', msg, 'Nelson:getdimarg:dimensionMustBePositiveInteger');
asserts.checkerror('diff(1:3, 1, 3)', msg, 'Nelson:getdimarg:dimensionMustBePositiveInteger');
%=============================================================================
asserts.checkerror('diff()', _('Not enough input arguments.'));
asserts.checkerror('diff(1:3, 1, 2, 4)', _('Too many input arguments.'));
asserts.checkerror('diff({1, 2})', _('Check for incorrect argument data type or missing argument in call to function ''minus''.'));
%=============================================================================
