%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
% Automatic contour levels are the multiples of one round step: the smallest
% of 1, 2 or 5 times a power of ten that cuts the data range into fewer than
% twelve slices. Every expected list below was measured, none was derived.
%=============================================================================
f = figure('Visible', 'off');
[~, h] = contour(peaks(5));
asserts.isequal(get(h, 'LevelList'), [-4 -2 0 2 4 6]);
asserts.isequal(get(gca(), 'CLim'), [-4 6]);
close(f);
%=============================================================================
% peaks(6) spans barely more than ten units and keeps a step of one: the step
% comes from the range, it is not a fixed multiple of anything.
f = figure('Visible', 'off');
[~, h] = contour(peaks(6));
asserts.isequal(get(h, 'LevelList'), [-4 -3 -2 -1 0 1 2 3 4 5]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
[~, h] = contour(peaks(20));
asserts.isequal(get(h, 'LevelList'), [-6 -4 -2 0 2 4 6]);
close(f);
%=============================================================================
% Integer data: a multiple landing on the data maximum is a level.
f = figure('Visible', 'off');
[~, h] = contour(magic(4));
asserts.isequal(get(h, 'LevelList'), [2 4 6 8 10 12 14 16]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
[~, h] = contour(magic(10));
asserts.isequal(get(h, 'LevelList'), [10 20 30 40 50 60 70 80 90 100]);
close(f);
%=============================================================================
% The step moves up exactly where a range of 11.9 becomes a range of 12.
f = figure('Visible', 'off');
[~, h] = contour(contour_ramp(0, 11.9));
asserts.isequal(get(h, 'LevelList'), 1:11);
close(f);
%=============================================================================
f = figure('Visible', 'off');
[~, h] = contour(contour_ramp(0, 12));
asserts.isequal(get(h, 'LevelList'), [2 4 6 8 10 12]);
close(f);
%=============================================================================
% The same frontier one decade up, where the step of five gives way to ten.
f = figure('Visible', 'off');
[~, h] = contour(contour_ramp(0, 55));
asserts.isequal(get(h, 'LevelList'), [5 10 15 20 25 30 35 40 45 50 55]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
[~, h] = contour(contour_ramp(0, 60));
asserts.isequal(get(h, 'LevelList'), [10 20 30 40 50 60]);
close(f);
%=============================================================================
% A fractional step comes back as the decimals it stands for.
f = figure('Visible', 'off');
[~, h] = contour(contour_ramp(0, 1));
asserts.isequal(get(h, 'LevelList'), [0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1]);
close(f);
%=============================================================================
% A level sitting on the data minimum is dropped when that minimum is zero or
% positive, and kept when it is negative.
f = figure('Visible', 'off');
[~, h] = contour(contour_ramp(0, 10));
asserts.isequal(get(h, 'LevelList'), 1:10);
close(f);
%=============================================================================
f = figure('Visible', 'off');
[~, h] = contour(contour_ramp(-1, 9));
asserts.isequal(get(h, 'LevelList'), -1:9);
close(f);
%=============================================================================
f = figure('Visible', 'off');
[~, h] = contour(contour_ramp(-11, 0));
asserts.isequal(get(h, 'LevelList'), -11:0);
close(f);
%=============================================================================
% A filled contour prepends the data minimum to that same list, and the
% colour limits follow the levels.
f = figure('Visible', 'off');
Z = peaks(5);
[~, h] = contourf(Z);
asserts.isequal(get(h, 'LevelList'), [min(Z(:)), -4, -2, 0, 2, 4, 6]);
asserts.isequal(get(gca(), 'CLim'), [min(Z(:)), 6]);
close(f);
%=============================================================================
% contour3 picks the levels of contour.
f = figure('Visible', 'off');
[~, h] = contour3(peaks(20));
asserts.isequal(get(h, 'LevelList'), [-6 -4 -2 0 2 4 6]);
close(f);
%=============================================================================
% fcontour shares the rule, and forces no level onto the sampled extremes.
f = figure('Visible', 'off');
h = fcontour(@(x, y) x .^ 2 + y .^ 2);
asserts.isequal(get(h, 'LevelList'), [5 10 15 20 25 30 35 40 45 50]);
close(f);
%=============================================================================
function Z = contour_ramp(a, b)
  Z = repmat(linspace(a, b, 21), 21, 1);
end
%=============================================================================
