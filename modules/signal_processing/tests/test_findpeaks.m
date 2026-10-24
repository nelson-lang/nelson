%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
data = [25 8 15 5 6 10 10 3 1 20 7];
pks = findpeaks(data);
REF_pks = [15 10 20];
asserts.isequal(pks, REF_pks);
%=============================================================================
plot(data);
findpeaks(data);
%=============================================================================
x = linspace(0, 1, 1000);
Pos = [1 2 3 5 7 8] / 10;
Hgt = [3 4 4 2 2 3];
Wdt = [2 6 3 3 4 6] / 100;
Gauss = zeros(length(Pos), length(x)); % preallocate properly
for n = 1:length(Pos)
  Gauss(n, :) = Hgt(n) * exp(-((x - Pos(n)) / Wdt(n)) .^ 2);
end
PeakSig = sum(Gauss, 1);
plot(x, Gauss, '--', x, PeakSig)
[pks, locs] = findpeaks(PeakSig, x);
findpeaks(PeakSig, x)
%=============================================================================
REF_pks = [3.2552 4.0000 4.2603 1.9994 2.2135 3.0039];
REF_locs = [0.1011 0.2002 0.2983 0.5005 0.7057 0.7998];
asserts.isapprox(pks, REF_pks, 1e-4);
asserts.isapprox(locs, REF_locs, 1e-4);
%=============================================================================
x = linspace(0, 1, 1000);
Pos = [1 2 3 5 7 8] / 10;
Hgt = [3 4 4 2 2 3];
Wdt = [2 6 3 3 4 6] / 100;
Gauss = zeros(length(Pos), length(x)); % preallocate properly
for n = 1:length(Pos)
  Gauss(n, :) = Hgt(n) * exp(-((x - Pos(n)) / Wdt(n)) .^ 2);
end
PeakSig = sum(Gauss, 1);
[psor, lsor] = findpeaks(PeakSig, x, 'SortStr', 'descend');
%=============================================================================
REF_psor = [4.2603 4.0000 3.2552 3.0039 2.2135 1.9994];
REF_lsor = [0.2983 0.2002 0.1011 0.7998 0.7057 0.5005];
asserts.isapprox(psor, REF_psor, 1e-4);
asserts.isapprox(lsor, REF_lsor, 1e-4);
%=============================================================================
rng('default');
fs = 1e2;
t = 0:1 / fs:1 - 1 / fs;
s = sin(2 * pi * 5 * t) .* sin(2 * pi * 3 * t) + randn(size(t)) / 10;
bnd = 0.32;
s(s > bnd) = bnd;
%=============================================================================
[pk, lc] = findpeaks(s, t);
%=============================================================================
REF_pk = [0.2413 0.3200 0.0327 0.2871 0.0888 0.0393 -0.1652 -0.3159 0.3200 0.3200 0.3200 0.2096 0.3200];
REF_lc = [0.0100 0.0300 0.1100 0.1800 0.3000 0.3200 0.3500 0.3800 0.4200 0.5300 0.6900 0.8300 0.9200];
asserts.isapprox(pk, REF_pk, 1e-3);
asserts.isapprox(lc, REF_lc, 1e-3);
%=============================================================================
[pkt, lct] = findpeaks(s, t, 'Threshold', 1e-4);
%=============================================================================
REF_pkt = [0.2413 0.0327 0.2871 0.0888 0.0393 -0.1652 -0.3159 0.3200 0.2096];
REF_lct = [0.0100 0.1100 0.1800 0.3000 0.3200 0.3500 0.3800 0.6900 0.8300];
asserts.isapprox(pkt, REF_pkt, 1e-3);
asserts.isapprox(lct, REF_lct, 1e-3);
%=============================================================================
x = linspace(0, 1, 1000);
Pos = [1 2 3 5 7 8] / 10;
Hgt = [4 4 2 2 2 3];
Wdt = [3 8 4 3 4 6] / 100;
Gauss = zeros(length(Pos), length(x)); % preallocate properly
for n = 1:length(Pos)
  Gauss(n, :) = Hgt(n) * exp(-((x - Pos(n)) / Wdt(n)) .^ 2);
end
plot(x, Gauss, '--', x, PeakSig);
grid on
findpeaks(PeakSig, x, 'Annotate', 'extents');
findpeaks(PeakSig, x, 'Annotate', 'extents', 'WidthReference', 'halfheight');
%=============================================================================