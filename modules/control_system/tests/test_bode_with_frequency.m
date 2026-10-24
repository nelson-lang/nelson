%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% bode(G, w) with an explicit frequency vector whose length differs from the
% internal default count must honour the user vector length.
%=============================================================================
G = tf([1], [1 1]);
w = logspace(-2, 2, 100);
%=============================================================================
% two output form
[mag, phase] = bode(G, w);
asserts.isequal(size(mag), [1 1 100]);
asserts.isequal(size(phase), [1 1 100]);
% parity with the analytic response of 1 / (j*w + 1)
magRef = 20 * log10(1 ./ sqrt(w .^ 2 + 1));
phaseRef = -atan(w) * 180 / pi;
asserts.isapprox(reshape(mag, 1, []), magRef, 1e-10);
asserts.isapprox(reshape(phase, 1, []), phaseRef, 1e-10);
asserts.isapprox(mag(1), -0.000434272769, 1e-4);
asserts.isapprox(mag(end), -40.000434272769, 1e-4);
asserts.isapprox(phase(1), -0.572938697683, 1e-4);
asserts.isapprox(phase(end), -89.427061302317, 1e-4);
%=============================================================================
% three output form, the returned frequency vector is the user one, sorted
[mag3, phase3, wout] = bode(G, w);
asserts.isequal(size(mag3), [1 1 100]);
asserts.isequal(size(phase3), [1 1 100]);
asserts.isequal(size(wout), [1 100]);
asserts.isequal(wout, sort(w));
%=============================================================================
% a frequency vector longer than the default internal count
wl = logspace(-2, 2, 1500);
[magl, phasel] = bode(G, wl);
asserts.isequal(size(magl), [1 1 1500]);
asserts.isequal(size(phasel), [1 1 1500]);
%=============================================================================
% a {wmin, wmax} cell interval keeps the default sampling count
[magc, phasec, woutc] = bode(G, {0.01, 100});
asserts.isequal(size(magc), [1 1 1000]);
asserts.isequal(size(woutc), [1 1000]);
%=============================================================================
% discrete time branch with an explicit frequency vector
Gd = tf([3 4], [3 1 5], 0.2);
wd = logspace(-2, 1, 100);
[magd, phased] = bode(Gd, wd);
asserts.isequal(size(magd), [1 1 100]);
asserts.isequal(size(phased), [1 1 100]);
% parity with a direct evaluation of the discrete transfer function
z = exp(1i * wd * 0.2);
Hd = polyval([3 4], z) ./ polyval([3 1 5], z);
magRefd = 20 * log10(abs(Hd));
asserts.isapprox(reshape(magd, 1, []), magRefd, 1e-10);
%=============================================================================
