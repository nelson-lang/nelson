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
%=============================================================================
% A color-mapped scatter (scalar CData per point) auto-scales CLim to the
% CData range, like surface/patch CData does.
f = figure('Visible', 'off');
scatter(1:10, 1:10, 50, (1:10), 'filled');
asserts.isequal(get(gca, 'CLim'), [1 10]);
close(f);
%=============================================================================
% A plain scatter with no color argument keeps the default CLim.
f = figure('Visible', 'off');
scatter(1:10, 1:10);
asserts.isequal(get(gca, 'CLim'), [0 1]);
close(f);
%=============================================================================
% True-color CData (Nx3 RGB) does not drive CLim.
f = figure('Visible', 'off');
rgb = repmat([1 0 0], 5, 1);
scatter(1:5, 1:5, 50, rgb, 'filled');
asserts.isequal(get(gca, 'CLim'), [0 1]);
close(f);
%=============================================================================
% A user-set CLim (CLimMode manual) is not overwritten by auto-scaling.
f = figure('Visible', 'off');
scatter(1:10, 1:10, 50, (1:10), 'filled');
clim([0 100]);
asserts.isequal(get(gca, 'CLim'), [0 100]);
close(f);
%=============================================================================
% surf keeps auto-scaling CLim to the Z / CData range (non-regression).
f = figure('Visible', 'off');
[X, Y] = meshgrid(-5:.5:5);
Z = X .^ 2 + Y .^ 2;
surf(Z);
asserts.isapprox(get(gca, 'CLim'), [0 50], 1e-4);
close(f);
%=============================================================================
% scatter3 with color-mapped CData also drives CLim.
f = figure('Visible', 'off');
scatter3(1:10, 1:10, 1:10, 50, (2:11), 'filled');
asserts.isequal(get(gca, 'CLim'), [2 11]);
close(f);
%=============================================================================
