%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% 'native' integer sum/prod saturate after each step, in index order.
asserts.isequal(sum(int8([100 100 -10]), 'native'), int8(117));
asserts.isequal(prod(int8([100 2 -1]), 'native'), int8(-127));
asserts.isequal(sum(uint8([200 100 0]), 'native'), uint8(255));
asserts.isequal(sum(int8([100 100 -10]), 'native', 'omitnan'), int8(117));
%=============================================================================
% default and 'double' outputs are unchanged
asserts.isequal(sum(int8([100 100 -10])), 190);
asserts.isequal(sum(int8([100 100 -10]), 'double'), 190);
asserts.isequal(prod(int8([100 2 -1])), -200);
asserts.isequal(sum(int64([2^53 1 1])), 2^53);
%=============================================================================
% every integer class
classes = {'int8', 'uint8', 'int16', 'uint16', 'int32', 'uint32', 'int64', 'uint64'};
for c = 1:numel(classes)
  cls = classes{c};
  mx = intmax(cls);
  mn = intmin(cls);
  one = cast(1, cls);
  ten = cast(10, cls);
  R = sum([mx mx -ten], 'native');
  asserts.isequal(class(R), cls);
  if mn < 0
    asserts.isequal(R, mx - ten);
    asserts.isequal(sum([mx - one, cast(2, cls), cast(-3, cls)], 'native'), mx - cast(3, cls));
    asserts.isequal(sum([mn mn ten], 'native'), mn + ten);
    if strcmp(cls, 'int8')
      asserts.isequal(prod(cast([100 2 -1], cls), 'native'), int8(-127));
    else
      asserts.isequal(prod(cast([100 2 -1], cls), 'native'), cast(-200, cls));
    end
    big = cast(floor(sqrt(double(mx))) + 1, cls);
    asserts.isequal(prod([big big -one], 'native'), -mx);
    asserts.isequal(prod([mn cast(2, cls) -one], 'native'), mx);
  else
    asserts.isequal(R, mx);
    asserts.isequal(sum([mx mx -ten], 'native'), mx);
    asserts.isequal(prod(cast([100 2 0], cls), 'native'), cast(0, cls));
  end
  asserts.isequal(sum(cast([1 2 3], cls)), 6);
end
%=============================================================================
% int64 'native' stays exact above flintmax
asserts.isequal(sum(int64([2^53 1 1]), 'native'), int64(9007199254740994));
asserts.isequal(sum([intmax('int64') int64(1) int64(-1)], 'native'), intmax('int64') - 1);
asserts.isequal(sum([intmax('int64') int64(-1) int64(1)], 'native'), intmax('int64'));
asserts.isequal(sum([intmax('uint64') uint64(1)], 'native'), intmax('uint64'));
%=============================================================================
% long vectors: same result as the last running sum
n = 300000;
k = 1:n;
long = {int8(mod(k * 7919, 251) - 120), int8(mod(k * 7919, 251) - 130), uint8(mod(k * 31, 256)), ...
        int16(mod(k * 7919, 65521) - 32000), int32(mod(k, 1000) - 499) * int32(4000000), ...
        uint32(mod(k * 13, 1000)) * uint32(40000), int64(mod(k, 1000) - 500) * int64(2e16), ...
        uint64(mod(k, 1000)) * uint64(4e16)};
for c = 1:numel(long)
  V = long{c};
  C = cumsum(V);
  asserts.isequal(sum(V, 'native'), C(end));
  asserts.isequal(sum(V.', 'native'), C(end));
  asserts.isequal(sum(V, 'all', 'native'), C(end));
  asserts.isequal(sum(reshape(V, 600, []), [1 2], 'native'), C(end));
end
signs = {int8(mod(k * 7919, 7) - 3), int16(mod(k * 13, 5) - 2), int32(1 - 2 * (mod(k, 3) == 0)), ...
         int64(mod(k * 7919, 7) - 3), uint8(1 + (mod(k, 50000) == 0)), uint32(mod(k, 3) + 1)};
for c = 1:numel(signs)
  P = signs{c};
  P(P == 0) = 1;
  C = cumprod(P);
  asserts.isequal(prod(P, 'native'), C(end));
  P(250000) = 0;
  C = cumprod(P);
  asserts.isequal(prod(P, 'native'), C(end));
end
P = [int8(2) int8(-ones(1, 299999))];
asserts.isequal(prod(P, 'native'), int8(-2));
P = [int8(100) int8(2) int8(-ones(1, 299999))];
asserts.isequal(prod(P, 'native'), int8(-127));
asserts.isequal(sum(uint16(mod(k, 4)), 'native'), intmax('uint16'));
asserts.isequal(sum(-int32(mod(k, 4)) * int32(2e4), 'native'), intmin('int32'));
V = [int8(ones(1, 200000)), int8(-ones(1, 100100))];
asserts.isequal(sum(V, 'native'), int8(-128));
V = [int8(ones(1, 200000)), int8(-ones(1, 100))];
asserts.isequal(sum(V, 'native'), int8(27));
V = int64(-ones(1, 200000)) * intmax('int64');
V(end) = intmax('int64');
asserts.isequal(sum(V, 'native'), int64(-1));
%=============================================================================
% dimensions and 'all'
M = int8([100 100; 100 -10; -10 -50]);
asserts.isequal(sum(M, 'native'), int8([117 40]));
asserts.isequal(sum(M, 1, 'native'), int8([117 40]));
asserts.isequal(sum(M, 2, 'native'), int8([127; 90; -60]));
asserts.isequal(sum(M, 'all', 'native'), int8(67));
asserts.isequal(sum(M, [1 2], 'native'), int8(67));
asserts.isequal(sum(M, [2 1], 'native'), int8(67));
asserts.isequal(prod(M, 'native'), int8([-128 127]));
asserts.isequal(prod(M, 2, 'native'), int8([127; -128; 127]));
asserts.isequal(prod(M, 'all', 'native'), int8(-128));
asserts.isequal(prod(M, [1 2], 'native'), int8(-128));
asserts.isequal(prod(M, [2 1], 'native'), int8(-128));
asserts.isequal(sum(int8([100; 100; -10]), 'native'), int8(117));
asserts.isequal(sum(int8([100 100 -10]), 2, 'native'), int8(117));
asserts.isequal(sum(int8([100 100 -10]), 1, 'native'), int8([100 100 -10]));
asserts.isequal(sum(int8([100 100 -10]), 3, 'native'), int8([100 100 -10]));
%=============================================================================
% vector of dimensions: consecutive dimensions are one line, smallest first
M3 = int8(reshape([100 100 -10 -100 50 50 -100 -100 1 2 3 4], 2, 3, 2));
asserts.isequal(sum(M3, [1 3], 'native'), int8([-1 -107 107]));
asserts.isequal(sum(M3, [3 1], 'native'), int8([-1 -107 107]));
asserts.isequal(sum(M3, [2 3], 'native'), int8([31; -44]));
asserts.isequal(prod(M3, [1 3], 'native'), int8([127 127 127]));
asserts.isequal(sum(M3, 3, 'native'), int8([0 -9 53; 0 -98 54]));
asserts.isequal(sum(M3, 'all', 'native'), int8(-73));
A = int8(reshape([100 100 -10 -100 50 50 -100 -100 1 2 3 4 90 90 -90 -90 5 6 7 8 120 -120 60 60], 2, 3, 4));
asserts.isequal(sum(A, [1 2], 'native'), int8(reshape([117 -118 -42 127], 1, 1, 4)));
asserts.isequal(sum(A, [2 1], 'native'), int8(reshape([117 -118 -42 127], 1, 1, 4)));
asserts.isequal(sum(A, [2 3], 'native'), int8([127; -68]));
asserts.isequal(sum(A, [3 2], 'native'), int8([127; -68]));
asserts.isequal(sum(A, [1 3], 'native'), int8([127 -128 127]));
asserts.isequal(sum(A, [1 2 3], 'native'), int8(73));
asserts.isequal(sum(A, [3 1 2], 'native'), int8(73));
asserts.isequal(sum(A, 'all', 'native'), int8(73));
asserts.isequal(prod(A, [1 2], 'native'), int8(reshape([127 127 127 -128], 1, 1, 4)));
asserts.isequal(prod(A, [2 3], 'native'), int8([-128; 127]));
asserts.isequal(prod(A, [1 3], 'native'), int8([127 -128 127]));
asserts.isequal(prod(A, [1 2 3], 'native'), int8(-128));
B = reshape(A, [2 3 2 2]);
asserts.isequal(sum(B, [1 2 4], 'native'), int8(reshape([75 9], 1, 1, 2)));
asserts.isequal(sum(B, [1 3 4], 'native'), int8([127 -128 127]));
asserts.isequal(sum(B, [2 4], 'native'), int8(reshape([127 56 31 -128], 2, 1, 2)));
asserts.isequal(sum(B, [1 2 3 4], 'native'), int8(73));
asserts.isequal(size(sum(A, [1 4], 'native')), [1 3 4]);
asserts.isequal(size(sum(A, [3 5], 'native')), [2 3]);
asserts.isequal(sum(A, [3 5], 'native'), sum(A, 3, 'native'));
asserts.isequal(sum(int8([100 100 -10]), [1 2], 'native'), int8(117));
asserts.isequal(sum(int8([100; 100; -10]), [2 1], 'native'), int8(117));
%=============================================================================
% empty inputs
asserts.isequal(sum(int8([]), 'native'), int8(0));
asserts.isequal(prod(int8([]), 'native'), int8(1));
asserts.isequal(sum(int8(zeros(0, 3)), 'native'), int8([0 0 0]));
asserts.isequal(prod(uint16(zeros(0, 3)), 'native'), uint16([1 1 1]));
asserts.isequal(sum(int8(zeros(3, 0)), 2, 'native'), int8([0; 0; 0]));
asserts.isequal(sum(int8([]), 'all', 'native'), int8(0));
asserts.isequal(sum(int8(zeros(0, 3)), 'all', 'native'), int8(0));
asserts.isequal(sum(int8([])), 0);
asserts.isequal(sum(zeros(0, 3, 'int8')), [0 0 0]);
%=============================================================================
% logical 'native' stays logical
asserts.isequal(sum([true true false], 'native'), true);
asserts.isequal(prod([true true false], 'native'), false);
asserts.isequal(prod([true true], 'native'), true);
asserts.isequal(sum(logical([]), 'native'), false);
asserts.isequal(prod(logical([]), 'native'), true);
asserts.isequal(sum(logical([1 1; 0 1]), 2, 'native'), [true; true]);
asserts.isequal(sum([true true]), 2);
%=============================================================================
% neighbours already following the same rule
asserts.isequal(mean(int8([100 100 -10]), 'native'), int8(63));
asserts.isequal(cumsum(int8([100 100 -10])), int8([100 127 117]));
asserts.isequal(cumprod(int8([100 2 -1])), int8([100 127 -127]));
%=============================================================================
% floating-point lines along any dimension add in index order, whatever the traversal
X = reshape(sin(1:6000) * 1e3, 40, 150);
X(7, 9) = NaN;
for f = {@sum, @prod}
  fn = f{1};
  asserts.isequal(fn(X, 2), fn(X.', 1).');
  asserts.isequal(fn(X, 2, 'omitnan'), fn(X.', 1, 'omitnan').');
  asserts.isequal(fn(single(X), 2), fn(single(X).', 1).');
  Z = X + 1i * cos(X);
  asserts.isequal(fn(Z, 2), fn(Z.', 1).');
  asserts.isequal(fn(Z, 2, 'omitnan'), fn(Z.', 1, 'omitnan').');
end
%=============================================================================
% cumulative and moving functions take one dimension only
msg = _('Dimension argument must be a positive integer scalar within indexing range.');
asserts.checkerror('cumsum(ones(2,3,4), [1 3])', msg, 'Nelson:getdimarg:dimensionMustBePositiveInteger');
asserts.checkerror('cumprod(ones(2,3,4), [1 3])', msg, 'Nelson:getdimarg:dimensionMustBePositiveInteger');
asserts.checkerror('movsum(ones(2,3,4), 2, [1 3])', msg, 'Nelson:getdimarg:dimensionMustBePositiveInteger');
%=============================================================================
