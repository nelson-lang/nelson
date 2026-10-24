%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
constructors = {@() zeros(1), @() ones(1), @() eye(1), @() nan(1), ...
  @() inf(1), @() double(single(3.5)), @() double(int64(-7)), ...
  @() double(true), @() double('A'), @() full(sparse(3.5)), ...
  @() sum([1.5 2]), @() prod([1 3.5]), @() reshape([3.5], [1 1]), ...
  @() diag(3.5), @() real(complex(3.5, 2)), @() imag(complex(2, 3.5))};
expected = [0, 1, 1, NaN, Inf, 3.5, -7, 1, 65, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5];
for k = 1:numel(constructors)
  value = constructors{k}();
  asserts.isequal(class(value), 'double');
  asserts.isequal(size(value), [1 1]);
  asserts.isfalse(issparse(value));
  asserts.istrue(isreal(value));
  asserts.istrue(isequaln(value, expected(k)));
  copy = value;
  value(1) = 19;
  asserts.istrue(isequaln(copy, expected(k)));
  value = copy;
  value(2) = 5;
  asserts.istrue(isequaln(value, [expected(k), 5]));
  asserts.istrue(isequaln(copy, expected(k)));
end
%=============================================================================
% These functions allocate their output before filling it.
[fraction, exponent] = log2(8);
asserts.isequal(fraction, 0.5);
asserts.isequal(exponent, 4);
asserts.isequal(abs(complex(3, 4)), 5);
asserts.isequal(hex2dec('FF'), 255);
asserts.isequal(bin2dec('101'), 5);
asserts.isequal(mod(8, 3), 2);
asserts.isequal(rem(8, 3), 2);
asserts.isequal(4 \ 12, 3);
%=============================================================================
negativeZero = full(sparse(-0.0));
asserts.isequal(negativeZero, 0);
negativeZero = double(single(-0.0));
asserts.isequal(1 / negativeZero, -Inf);
asserts.istrue(isnan(double(single(NaN))));
asserts.isequal(double(single(Inf)), Inf);
%=============================================================================
% Empty arrays, non-scalars, sparse values and complex values retain their class.
asserts.isequal(size(zeros(0, 1)), [0 1]);
asserts.isequal(zeros(1, 2), [0 0]);
asserts.isequal(ones(2, 1), [1; 1]);
asserts.isequal(size(reshape(1:2, [1 1 2])), [1 1 2]);
asserts.istrue(issparse(sparse(3.5)));
asserts.isfalse(isreal(complex(3.5, 2)));
asserts.isequal(class(single(3.5)), 'single');
%=============================================================================
