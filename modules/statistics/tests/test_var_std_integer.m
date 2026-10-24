%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% var: integer input data is computed in double
V = var(int8([1 2 3 4]));
assert_isequal(class(V), 'double');
assert_isapprox(V, 5 / 3, 1e-12);
%=============================================================================
% no saturation of the integer class
assert_isequal(var(int8([-128 127])), 32512.5);
assert_isequal(var(uint8([0 255])), 32512.5);
assert_isequal(var(uint32([0 4294967295])), 4294967295 ^ 2 / 2);
assert_isequal(var(int32([-2147483648 2147483647])), 4294967295 ^ 2 / 2);
%=============================================================================
% every supported integer class
classes = {'int8', 'int16', 'int32', 'int64', 'uint8', 'uint16', 'uint32', 'uint64'};
for k = 1:numel(classes)
  X = cast([4 1 10; 7 4 7; 3 2 9], classes{k});
  Xd = double(X);
  assert_isequal(var(X), var(Xd));
  assert_isequal(std(X), std(Xd));
  assert_isequal(class(var(X)), 'double');
  assert_isequal(class(std(X)), 'double');
end
%=============================================================================
% options: normalization, weights, dim, vecdim, 'all', nanflag
X = int16([4 -7 3; 1 4 -2; 10 7 9]);
Xd = double(X);
assert_isequal(var(X, 1), var(Xd, 1));
assert_isequal(var(X, 0, 2), var(Xd, 0, 2));
assert_isequal(var(X, [1 2 3]), var(Xd, [1 2 3]));
assert_isequal(var(X, [], 'all'), var(Xd, [], 'all'));
assert_isequal(var(X, 0, [1 2]), var(Xd, 0, [1 2]));
assert_isequal(var(X, 'omitnan'), var(Xd, 'omitnan'));
assert_isequal(std(X, 1, 2), std(Xd, 1, 2));
assert_isequal(std(X, [3 2 1]), std(Xd, [3 2 1]));
assert_isequal(std(X, 0, 'all'), std(Xd, 0, 'all'));
assert_isequal(std(X, 0, [1 2]), std(Xd, 0, [1 2]));
%=============================================================================
% std: second output (mean) is double
[S, M] = std(uint8([10 20 255]));
assert_isequal(class(S), 'double');
assert_isequal(class(M), 'double');
assert_isapprox(S, sqrt(19225), 1e-12);
assert_isapprox(M, 95, 1e-12);
[S, M] = std(int8([1 2; 3 5]), [1 3]);
[Sd, Md] = std([1 2; 3 5], [1 3]);
assert_isequal(S, Sd);
assert_isequal(M, Md);
assert_isequal(class(M), 'double');
%=============================================================================
% scalar and empty integer input
assert_isequal(var(int8(5)), 0);
assert_isequal(std(uint16(5)), 0);
assert_isequal(var(int8([])), NaN);
%=============================================================================
% int64 and uint64 data is converted to double
assert_isequal(var(int64([1 2 3])), 1);
assert_isequal(class(var(int64([1 2 3]))), 'double');
assert_isequal(std(uint64([1 2 3])), 1);
assert_isequal(class(std(uint64([1 2 3]))), 'double');
v = 2 ^ 53;
assert_isequal(var(int64([-v v])), 2 ^ 107);
assert_isequal(var(uint64([0 intmax('uint64')])), 2 ^ 127);
[S, M] = std(uint64([0 intmax('uint64')]));
assert_isapprox(S, sqrt(2) * 2 ^ 63, 1e-15);
assert_isequal(M, 2 ^ 63);
%=============================================================================
