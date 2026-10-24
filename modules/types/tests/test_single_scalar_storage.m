%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
constructors = {@() single(3.5), @() single(-7), @() single(true), ...
  @() zeros(1, 'single'), @() ones(1, 'single'), @() nan(1, 'single'), ...
  @() inf(1, 'single'), @() single(int64(-7)), @() sum(single([1 2.5])), ...
  @() prod(single([1 3.5])), @() real(complex(single(3.5), single(2))), ...
  @() imag(complex(single(2), single(3.5))), @() abs(complex(single(3), single(4)))};
expected = single([3.5 -7 1 0 1 NaN Inf -7 3.5 3.5 3.5 3.5 5]);
for k = 1:numel(constructors)
  value = constructors{k}();
  asserts.isequal(class(value), 'single');
  asserts.isequal(size(value), [1 1]);
  asserts.istrue(isequaln(value, expected(k)));
  asserts.istrue(isfloat(value));
  asserts.istrue(isreal(value));
  copy = value;
  value(1) = single(19);
  asserts.istrue(isequaln(copy, expected(k)));
  value = copy;
  value(2) = single(5);
  asserts.istrue(isequaln(value, [expected(k), single(5)]));
  asserts.isequal(class(copy(false)), 'single');
  asserts.istrue(isempty(copy(false)));
  asserts.isequal(class(copy([])), 'single');
  asserts.istrue(isequaln(copy(true), copy));
  asserts.istrue(isequaln(copy(single(1)), copy));
  asserts.istrue(isequaln(copy(1, 1), copy));
  asserts.isequal(class(double(copy)), 'double');
  if ~isnan(copy)
    asserts.isequal(class(logical(copy)), 'logical');
  end
end
%=============================================================================
asserts.isequal(double(single(3.5)), 3.5);
asserts.isequal(logical(single(0)), false);
asserts.isequal(logical(single(-3.5)), true);
asserts.isequal(single(2) + single(3), single(5));
asserts.isequal(single(2) * single(3), single(6));
asserts.isequal(sqrt(single(9)), single(3));
[fraction, exponent] = log2(single(8));
asserts.isequal(fraction, single(0.5));
asserts.isequal(exponent, single(4));
negativeZero = single(-0.0);
asserts.isequal(single(1) ./ negativeZero, single(-Inf));
asserts.isequal(single(1) ./ single(0.0), single(Inf));
asserts.istrue(isnan(single(NaN)));
%=============================================================================
value = {single(3.5)};
copy = value;
value{1}(1) = single(7);
asserts.isequal(copy{1}, single(3.5));
value = struct('number', single(3.5));
copy = value;
value.number(1) = single(7);
asserts.isequal(copy.number, single(3.5));
asserts.isequal(size(zeros(0, 1, 'single')), [0 1]);
asserts.isequal(ones(1, 2, 'single'), single([1 1]));
asserts.isequal(size(reshape(single(1:2), [1 1 2])), [1 1 2]);
asserts.isfalse(isreal(complex(single(1), single(2))));
%=============================================================================
