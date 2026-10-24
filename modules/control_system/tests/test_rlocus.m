%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
% Branches start at the open-loop poles and the result is well shaped.
G = tf(1, [1 3 2]);
[r, k] = rlocus(G);
den = [1 3 2];
n = numel(den) - 1;
asserts.isequal(size(r), [n, numel(k)]);
asserts.istrue(k(1) == 0);
% First column (k = 0) must equal the open-loop poles (roots of den).
startPoles = sort(r(:, 1));
olpol = sort(roots(den));
asserts.isapprox(real(startPoles), real(olpol), 1e-9);
asserts.isapprox(imag(startPoles), imag(olpol), 1e-9);
%=============================================================================
% Real coefficients give a conjugate-symmetric set of poles at every gain.
for col = 1:size(r, 2)
  poles = sort(r(:, col));
  conjPoles = sort(conj(r(:, col)));
  asserts.isapprox(real(poles), real(conjPoles), 1e-9);
  asserts.isapprox(imag(poles), imag(conjPoles), 1e-9);
end
%=============================================================================
% Exact internal oracle: closed-loop poles at a specific gain equal
% roots(den + k*num) computed directly.
G = tf(1, [1 3 2]);
kTest = 5;
[r, k] = rlocus(G, kTest);
expected = sort(roots([1 3 2] + kTest * [0 0 1]));
got = sort(r(:, 1));
asserts.isapprox(real(got), real(expected), 1e-9);
asserts.isapprox(imag(got), imag(expected), 1e-9);
%=============================================================================
% With k = 5 on 1/(s^2+3s+2): den + 5 = s^2 + 3s + 7, poles -1.5 +/- j*sqrt(19)/2.
asserts.isapprox(sort(real(got)), [-1.5; -1.5], 1e-9);
asserts.isapprox(sort(abs(imag(got))), sort(abs([1; -1] * sqrt(19) / 2)), 1e-9);
%=============================================================================
% A finite zero attracts one branch as the gain grows.
G = tf([1 3], [1 3 2]);  % open-loop zero at s = -3
num = [0 1 3];
den = [1 3 2];
[r, k] = rlocus(G);
% Closed-loop poles at the largest gain: at least one must be near the zero -3.
lastPoles = r(:, end);
[~, idx] = min(abs(lastPoles - (-3)));
asserts.istrue(abs(lastPoles(idx) - (-3)) < 1e-2);
%=============================================================================
% Supplying a gain vector returns one column per gain, matching the oracle.
kvec = [0 1 2 10 100];
[r, k] = rlocus(G, kvec);
asserts.isequal(size(r), [2, numel(kvec)]);
asserts.isequal(k, kvec);
for col = 1:numel(kvec)
  expected = sort(roots(den + kvec(col) * num));
  got = sort(r(:, col));
  asserts.isapprox(real(got), real(expected), 1e-9);
  asserts.isapprox(imag(got), imag(expected), 1e-9);
end
%=============================================================================
% The auto gain grid includes the breakaway gain so the loci turn crisply.
% For 1/(s^2+3s+2) the breakaway is at s = -1.5 with gain k = 0.25, where both
% closed-loop poles coincide at -1.5.
G = tf(1, [1 3 2]);
[r, k] = rlocus(G);
[gap, idx] = min(abs(k - 0.25));
asserts.istrue(gap < 1e-6);
breakawayPoles = r(:, idx);
asserts.isapprox(real(breakawayPoles), [-1.5; -1.5], 1e-5);
asserts.isapprox(imag(breakawayPoles), [0; 0], 1e-5);
%=============================================================================
% No-output call plots without error (offscreen figure).
f = figure('Visible', 'off');
rlocus(tf(1, [1 3 2]));
close(f);
%=============================================================================
% A model that is not SISO is rejected.
M = [1, 2, 3, 4; 5 6 7 8; 8 9 10 11];
sys = tf(M);
asserts.checkerror('r = rlocus(sys);', sprintf(_('%s expected.'), 'SISO LTI model'));
%=============================================================================
