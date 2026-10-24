%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
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
close all
%=============================================================================
f = figure('Visible', 'off', 'InnerPosition', [100 100 420 320], 'Color', [1 1 1]);
ax = axes('Parent', f, 'Position', [0.12 0.12 0.78 0.78]);
[X, Y, Z] = peaks(45);
surf(ax, X, Y, Z, 'EdgeColor', 'none');
set(ax, 'Box', 'off', 'XGrid', 'off', 'YGrid', 'off', 'ZGrid', 'off', ...
  'XTickLabel', {}, 'YTickLabel', {}, 'ZTickLabel', {}, 'LineWidth', 1.5);
view(ax, [-37.5 30]);
drawnow();
boxOffFrame = getframe(f).cdata;
ax.Box = 'on';
drawnow();
boxOnFrame = getframe(f).cdata;

surfaceRows = 85:235;
surfaceColumns = 95:330;
boxOffCrop = boxOffFrame(surfaceRows, surfaceColumns, :);
boxOnCrop = boxOnFrame(surfaceRows, surfaceColumns, :);
addedDarkPixels = dark_pixel_mask(boxOnCrop) & ~dark_pixel_mask(boxOffCrop);

asserts.istrue(nnz(dark_pixel_mask(boxOnFrame) & ~dark_pixel_mask(boxOffFrame)) > 50);
asserts.istrue(nnz(addedDarkPixels) / numel(addedDarkPixels) < 0.015);
close(f);
%=============================================================================
function mask = dark_pixel_mask(frame)
  mask = frame(:, :, 1) < 45 & frame(:, :, 2) < 45 & frame(:, :, 3) < 45;
end
%=============================================================================
