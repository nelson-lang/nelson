%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('arrayfun'), -1);
asserts.isequal(nargout('arrayfun'), -1);
%=============================================================================
R = arrayfun(@(x) x .^ 2, [1 2; 3 4]);
asserts.isequal(R, [1 4; 9 16]);
%=============================================================================
R = arrayfun(@(x)[x x + 1], 1:3, 'UniformOutput', false);
asserts.isequal(R, {[1 2], [2 3], [3 4]});
%=============================================================================
[A, B] = arrayfun(@(x) deal(x, x + 10), 1:3);
asserts.isequal(A, 1:3);
asserts.isequal(B, 11:13);
%=============================================================================
Squared = arrayfun(@(x) x .^ 2, [1 2; 3 4]);
asserts.isequal(Squared, [1 4; 9 16]);
NonUniform = arrayfun(@(x)[x x + 1], 1:3, 'UniformOutput', false);
asserts.isequal(NonUniform, {[1 2], [2 3], [3 4]});
[ArrayA, ArrayB] = arrayfun(@(x) deal(x, x + 10), 1:3);
asserts.isequal(ArrayA, 1:3);
asserts.isequal(ArrayB, 11:13);
errorHandled = arrayfun(@(x) str2num(x), 1:3, 'ErrorHandler', @(s, x) - 1);
asserts.isequal(errorHandled, [-1 -1 -1]);
asserts.checkerror('arrayfun(@(x) [x x], 1:2)', _('Non-scalar in Uniform output.'));
%=============================================================================
% uniform collection of scalar string outputs (must not corrupt/crash)
r = arrayfun(@(x) string(x), [1 2 3]);
assert_isequal(r, ["1" "2" "3"]);
r = arrayfun(@(x) "val" + x, [1 2 3]);
assert_isequal(r, ["val1" "val2" "val3"]);
r = arrayfun(@(x) lower(x), ["ABC" "def" "GHI"]);
assert_isequal(r, ["abc" "def" "ghi"]);
%=============================================================================
% an empty input gives an empty output of the same size
R = arrayfun(@(x) x * 2, zeros(0, 3));
asserts.isequal(size(R), [0 3]);
asserts.isequal(class(R), 'double');
R = arrayfun(@(x) x * 2, zeros(3, 0));
asserts.isequal(size(R), [3 0]);
R = arrayfun(@(x) x * 2, zeros(0, 0, 2));
asserts.isequal(size(R), [0 0 2]);
R = arrayfun(@(x) x.a, repmat(struct('a', 1), 1, 0));
asserts.isequal(size(R), [1 0]);
%=============================================================================
% 'ErrorHandler' and 'UniformOutput' together, in both orders
R = arrayfun(@(x) str2num(x), [1 2], 'ErrorHandler', @(e, x) e.index, 'UniformOutput', false);
asserts.isequal(R, {1, 2});
R = arrayfun(@(x) str2num(x), [1 2], 'UniformOutput', false, 'ErrorHandler', @(e, x) [x x]);
asserts.isequal(R, {[1 1], [2 2]});
R = arrayfun(@(x) str2num(x), 1, 'ErrorHandler', @(e, x) fieldnames(e), 'UniformOutput', false);
asserts.isequal(R, {{'identifier'; 'message'; 'index'}});
%=============================================================================
