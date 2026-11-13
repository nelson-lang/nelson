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
% High-level image: manual limits fitted to the image, reversed y, ticks on top.
f = figure('Visible', 'off');
ax = axes(f);
image(ax, magic(4));
asserts.isequal(get(ax, 'XLimMode'), 'manual');
asserts.isequal(get(ax, 'YLimMode'), 'manual');
asserts.isequal(get(ax, 'ZLimMode'), 'auto');
asserts.isequal(get(ax, 'XLim'), [0.5 4.5]);
asserts.isequal(get(ax, 'YLim'), [0.5 4.5]);
asserts.isequal(get(ax, 'YDir'), 'reverse');
asserts.isequal(get(ax, 'Layer'), 'top');
asserts.isequal(char(get(ax, 'Box')), 'on');
asserts.isequal(get(ax, 'XLimitMethod'), 'tickaligned');
%=============================================================================
% Changing CData later keeps the manual limits.
h = ax.Children(1);
h.CData = magic(8);
asserts.isequal(get(ax, 'XLim'), [0.5 4.5]);
%=============================================================================
% A new high-level image replaces the limits.
image(ax, magic(8));
asserts.isequal(get(ax, 'XLim'), [0.5 8.5]);
%=============================================================================
% Explicit X and Y data.
image(ax, [10 20], [5 8], magic(4));
asserts.isequal(get(ax, 'XLimMode'), 'manual');
asserts.isapprox(get(ax, 'XLim'), [25 / 3, 65 / 3], 1e-12);
asserts.isequal(get(ax, 'YLim'), [4.5 8.5]);
%=============================================================================
% Releasing one limit keeps the other manual.
image(ax, magic(4));
xlim(ax, 'auto');
asserts.isequal(get(ax, 'XLimMode'), 'auto');
asserts.isequal(get(ax, 'YLimMode'), 'manual');
delete(f);
%=============================================================================
% Under hold the axes setup is left untouched.
f = figure('Visible', 'off');
ax = axes(f);
plot(ax, 1:20);
hold(ax, 'on');
image(ax, magic(4));
asserts.isequal(get(ax, 'XLimMode'), 'auto');
asserts.isequal(get(ax, 'YDir'), 'normal');
asserts.isequal(get(ax, 'Layer'), 'bottom');
asserts.isequal(get(ax, 'XLim'), [0 20]);
delete(f);
%=============================================================================
% A second image under hold keeps the limits of the first one.
f = figure('Visible', 'off');
ax = axes(f);
image(ax, magic(4));
hold(ax, 'on');
image(ax, [10 20], [5 8], magic(4));
asserts.isequal(get(ax, 'XLim'), [0.5 4.5]);
delete(f);
%=============================================================================
% NextPlot 'replacechildren' also leaves the axes setup alone.
f = figure('Visible', 'off');
ax = axes(f);
ax.NextPlot = 'replacechildren';
image(ax, magic(4));
asserts.isequal(get(ax, 'XLimMode'), 'auto');
asserts.isequal(get(ax, 'YDir'), 'normal');
delete(f);
%=============================================================================
% Low-level image keeps auto limits.
f = figure('Visible', 'off');
ax = axes(f);
image(ax, 'CData', magic(4));
asserts.isequal(get(ax, 'XLimMode'), 'auto');
asserts.isequal(get(ax, 'YDir'), 'normal');
delete(f);
%=============================================================================
% imagesc follows the same rules; its property form is low-level.
f = figure('Visible', 'off');
ax = axes(f);
imagesc(ax, magic(4));
asserts.isequal(get(ax, 'XLimMode'), 'manual');
asserts.isequal(get(ax, 'YLimMode'), 'manual');
asserts.isequal(get(ax, 'XLim'), [0.5 4.5]);
asserts.isequal(get(ax, 'YDir'), 'reverse');
asserts.isequal(get(ax, 'Layer'), 'top');
delete(f);
f = figure('Visible', 'off');
ax = axes(f);
imagesc(ax, 'CData', magic(4));
asserts.isequal(get(ax, 'XLimMode'), 'auto');
asserts.isequal(get(ax, 'YDir'), 'normal');
asserts.isequal(char(get(ax, 'Box')), 'off');
delete(f);
f = figure('Visible', 'off');
ax = axes(f);
plot(ax, 1:20);
hold(ax, 'on');
imagesc(ax, magic(4));
asserts.isequal(get(ax, 'XLimMode'), 'auto');
asserts.isequal(get(ax, 'YDir'), 'normal');
delete(f);
%=============================================================================
% imshow pins its limits; under hold it only sets the aspect ratio and direction.
f = figure('Visible', 'off');
ax = axes(f);
imshow(magic(4), [], 'Parent', ax);
asserts.isequal(get(ax, 'XLimMode'), 'manual');
asserts.isequal(get(ax, 'XLim'), [0.5 4.5]);
asserts.isequal(get(ax, 'YLim'), [0.5 4.5]);
asserts.isequal(get(ax, 'DataAspectRatio'), [1 1 1]);
asserts.isequal(get(ax, 'Layer'), 'top');
asserts.isequal(get(ax, 'XLimitMethod'), 'tickaligned');
delete(f);
f = figure('Visible', 'off');
ax = axes(f);
plot(ax, 1:20);
hold(ax, 'on');
imshow(magic(4), [], 'Parent', ax);
asserts.isequal(get(ax, 'XLimMode'), 'auto');
asserts.isequal(get(ax, 'XLim'), [0 20]);
asserts.isequal(get(ax, 'DataAspectRatio'), [1 1 1]);
asserts.isequal(get(ax, 'YDir'), 'reverse');
asserts.isequal(get(ax, 'Layer'), 'bottom');
delete(f);
%=============================================================================
