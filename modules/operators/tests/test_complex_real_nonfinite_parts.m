%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A complex array times or divided by a finite nonzero real keeps infinite parts.
for c = {'double', 'single'}
  a = complex(cast([1 NaN Inf 2], c{1}), cast([-Inf 0 0 NaN], c{1}));
  for k = {180, cast(180, c{1})}
    R = a .* k{1};
    assert_isequal(class(R), c{1});
    assert_isequal(real(R(1:3)), cast([180 NaN Inf], c{1}));
    assert_isequal(imag(R(1:3)), cast([-Inf 0 0], c{1}));
    assert_isequal(isnan(real(R(4))) && isnan(imag(R(4))), true);
    R = k{1} .* a;
    assert_isequal(real(R(1:3)), cast([180 NaN Inf], c{1}));
    assert_isequal(imag(R(1:3)), cast([-Inf 0 0], c{1}));
  end
  R = a ./ pi;
  assert_isequal(class(R), c{1});
  assert_isequal(real(R(1:3)), cast([1 NaN Inf], c{1}) ./ cast(pi, c{1}));
  assert_isequal(imag(R(1:3)), cast([-Inf 0 0], c{1}));
  % function forms give the same result for scalars
  R = times(complex(cast(1, c{1}), -Inf), 180);
  assert_isequal(real(R), cast(180, c{1}));
  assert_isequal(imag(R), cast(-Inf, c{1}));
  R = rdivide(complex(cast(Inf, c{1}), 1), 2);
  assert_isequal(real(R), cast(Inf, c{1}));
  assert_isequal(imag(R), cast(0.5, c{1}));
end
%=============================================================================
% zero divisor or factor keeps the generic rules
R = complex([1 2], [1 Inf]) .* [2 0];
assert_isequal(R(1), complex(2, 2));
%=============================================================================
% zero to a negative power is infinite, -0 to an odd negative power is -Inf
assert_isequal([-1 0 -0 2] .^ -1, [-1 Inf -Inf 0.5]);
assert_isequal([-1 0 -0 2] .^ -2, [1 Inf Inf 0.25]);
assert_isequal(single([-1 0 -0 2]) .^ -1, single([-1 Inf -Inf 0.5]));
R = [-Inf 4] .^ 0.5;
assert_isequal(real(R), [0 2]);
assert_isequal(imag(R), [Inf 0]);
R = (-Inf) .^ (1 / 3);
assert_isequal(real(R) == Inf && imag(R) == Inf, true);
%=============================================================================
