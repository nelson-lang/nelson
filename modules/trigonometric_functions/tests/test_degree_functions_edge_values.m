%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
isNegZero = @(v) v == 0 & 1 ./ v < 0;
isPosZero = @(v) v == 0 & 1 ./ v > 0;
%=============================================================================
for c = {'double', 'single'}
  x = cast([-0 0 90 180 270 360 -90 -180 -270 -360 Inf -Inf NaN], c{1});
  % exact values, zeros of sind carry the sign of x, zeros of cosd are +0
  S = sind(x);
  assert_isequal(class(S), c{1});
  assert_isequal(S, cast([0 0 1 0 -1 0 -1 0 1 0 NaN NaN NaN], c{1}));
  assert_isequal(isNegZero(S([1 8 10])), [true true true]);
  assert_isequal(isPosZero(S([2 4 6])), [true true true]);
  C = cosd(x);
  assert_isequal(C, cast([1 1 0 -1 0 1 0 -1 0 1 NaN NaN NaN], c{1}));
  assert_isequal(isPosZero(C([3 5 7 9])), [true true true true]);
  T = tand(x);
  assert_isequal(T, cast([0 0 Inf 0 -Inf 0 -Inf 0 Inf 0 NaN NaN NaN], c{1}));
  assert_isequal(isNegZero(T([1 4 10])), [true true true]);
  assert_isequal(cscd(x(1:10)), cast([-Inf Inf 1 Inf -1 Inf -1 -Inf 1 -Inf], c{1}));
  assert_isequal(cotd(x(1:10)), cast([-Inf Inf 0 -Inf 0 Inf 0 Inf 0 -Inf], c{1}));
  assert_isequal(secd(x(1:10)), cast([1 1 Inf -1 Inf 1 Inf -1 Inf 1], c{1}));
  % common angles
  assert_isequal(sind(cast([30 150 -30 45], c{1})), cast([0.5 0.5 -0.5 sqrt(0.5)], c{1}));
  assert_isequal(tand(cast([45 135 -45 225], c{1})), cast([1 -1 -1 1], c{1}));
  % inverse functions in degrees at 0, Inf and NaN
  y = cast([-0 0 Inf -Inf NaN 2], c{1});
  R = asind(y);
  assert_isequal(real(R), cast([0 0 90 -90 NaN 90], c{1}));
  assert_isequal(imag(R(1:5)), cast([0 0 -Inf Inf 0], c{1}));
  assert_isequal(isNegZero(real(R(1))), true);
  R = acosd(y);
  assert_isequal(real(R), cast([90 90 0 180 NaN 0], c{1}));
  assert_isequal(imag(R(1:5)), cast([0 0 Inf -Inf 0], c{1}));
  R = asecd(y);
  assert_isequal(real(R(1:5)), cast([180 0 90 90 NaN], c{1}));
  assert_isequal(imag(R(1:5)), cast([-Inf Inf 0 0 0], c{1}));
  R = acscd(y);
  assert_isequal(real(R(1:5)), cast([-90 90 0 0 NaN], c{1}));
  assert_isequal(imag(R(1:5)), cast([Inf -Inf 0 0 0], c{1}));
end
%=============================================================================
% scalar calls follow the same rules
assert_isequal(asind(Inf), complex(90, -Inf));
assert_isequal(acosd(-Inf), complex(180, -Inf));
assert_isequal(asecd(-0), complex(180, -Inf));
assert_isequal(acscd(0), complex(90, -Inf));
assert_isequal(cscd(-0), -Inf);
assert_isequal(cotd(-0), -Inf);
%=============================================================================
% exact reduction of large angles
assert_isequal(sind([1e300 -1e300]), [0 0]);
assert_isequal(isNegZero(sind(-1e300)), true);
assert_isequal(cosd(1e300), 1);
assert_isequal(sind(1e10 + 30), sind(30 + 280));
%=============================================================================
% complex input
z = complex([30 -0 0.5 NaN], [1 Inf -Inf 0]);
S = sind(z);
assert_isapprox(S(1), sin(z(1) * pi / 180), 1e-12);
assert_isequal(isNegZero(real(S(2))) && imag(S(2)) == Inf, true);
assert_isequal(real(S(3)) == Inf && imag(S(3)) == -Inf, true);
assert_isequal(isnan(real(S(4))) && imag(S(4)) == 0, true);
C = cosd(z);
assert_isapprox(C(1), cos(z(1) * pi / 180), 1e-12);
assert_isequal(real(C(2)) == Inf && isPosZero(imag(C(2))), true);
%=============================================================================
% sparse input
assert_isequal(full(sind(sparse([0 30 180]))), [0 0.5 0]);
assert_isequal(full(cosd(sparse([0 60 90]))), [1 0.5 0]);
%=============================================================================
