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
% <--RELEASE ONLY-->
%=============================================================================
skip_if_portable_graphics_backend('native figure windows are not available.');
% A wide line drawn before must not make the axes background (Color) bigger
% than the plot box: the fill was stroked with that line's pen.
f = figure('Visible', 'off', 'Position', [100 100 600 400]);
ax1 = polaraxes('Parent', f, 'Position', [0.05 0.1 0.4 0.8]);
polarplot(ax1, linspace(0, pi / 2, 50), 2 * ones(1, 50), 'LineWidth', 12);
ax2 = axes('Parent', f, 'Position', [0.55 0.1 0.4 0.8]);
ax2.Color = [1 0 0];
ax2.XTick = [];
ax2.YTick = [];
fr = getframe(f);
close(f);
%=============================================================================
img = double(fr.cdata);
isRed = img(:, :, 1) > 200 & img(:, :, 2) < 80 & img(:, :, 3) < 80;
[rows, cols] = find(isRed);
assert_isfalse(isempty(rows));
[h, w, ~] = size(img);
% Expected plot box in pixels from the normalized Position.
expectedLeft = 0.55 * w;
expectedRight = 0.95 * w;
expectedTop = (1 - 0.9) * h;
expectedBottom = (1 - 0.1) * h;
tol = 3;
assert_istrue(min(cols) >= expectedLeft - tol);
assert_istrue(max(cols) <= expectedRight + tol);
assert_istrue(min(rows) >= expectedTop - tol);
assert_istrue(max(rows) <= expectedBottom + tol);
%=============================================================================
% An opaque image filling the plot box leaves no background row or column visible.
f = figure('Visible', 'off', 'Position', [100 100 600 400]);
ax = axes('Parent', f, 'Position', [0.3 0.1 0.4 0.8]);
imagesc(ax, magic(6));
ax.Color = [1 0 0];
fr = getframe(f);
close(f);
img = double(fr.cdata);
isRed = img(:, :, 1) > 200 & img(:, :, 2) < 80 & img(:, :, 3) < 80;
assert_isequal(nnz(isRed), 0);
%=============================================================================
