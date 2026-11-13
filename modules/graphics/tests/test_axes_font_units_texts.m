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
% Axes FontUnits change: values measured on the reference.
close all
f = figure('Visible', 'off');
ax = axes(f);
xlabel(ax, 'x'); ylabel(ax, 'y'); zlabel(ax, 'z'); title(ax, 't'); subtitle(ax, 's');
ax.XLabel.FontSize = 17;
ax.LabelFontSizeMultiplier = 1.5;
ax.Title.FontSize = 18;
ax.TitleFontSizeMultiplier = 2;
ax.FontUnits = 'pixels';
assert_isapprox(ax.FontSize, 40 / 3, 1e-12);
% Ruler labels keep points.
assert_isequal(ax.XLabel.FontUnits, 'points');
assert_isequal(ax.XLabel.FontSize, 17);
assert_isequal(ax.XLabel.FontSizeMode, 'manual');
assert_isequal(ax.YLabel.FontUnits, 'points');
assert_isequal(ax.YLabel.FontSize, 15);
% Title, subtitle and the 2D ZLabel follow the axes units.
assert_isequal(ax.ZLabel.FontUnits, 'pixels');
assert_isapprox(ax.ZLabel.FontSize, 40 / 3, 1e-12);
assert_isequal(ax.Title.FontUnits, 'pixels');
assert_isapprox(ax.Title.FontSize, 24, 1e-12);
assert_isequal(ax.Title.FontSizeMode, 'manual');
assert_isequal(ax.Subtitle.FontUnits, 'pixels');
assert_isapprox(ax.Subtitle.FontSize, 80 / 3, 1e-12);
assert_isequal(ax.Subtitle.FontSizeMode, 'auto');
ax.FontUnits = 'points';
assert_isapprox(ax.FontSize, 10, 1e-12);
assert_isequal(ax.ZLabel.FontUnits, 'points');
assert_isapprox(ax.ZLabel.FontSize, 10, 1e-12);
assert_isapprox(ax.Title.FontSize, 18, 1e-12);
assert_isapprox(ax.Subtitle.FontSize, 20, 1e-12);
assert_isequal(ax.XLabel.FontSize, 17);
%=============================================================================
% A later axes FontSize is written in each text's own units.
ax.FontUnits = 'pixels';
ax.FontSize = 20;
assert_isapprox(ax.XLabel.FontSize, 22.5, 1e-12);
assert_isequal(ax.XLabel.FontUnits, 'points');
assert_isequal(ax.XLabel.FontSizeMode, 'auto');
assert_isapprox(ax.YLabel.FontSize, 22.5, 1e-12);
assert_isapprox(ax.Title.FontSize, 40, 1e-12);
assert_isequal(ax.Title.FontUnits, 'pixels');
assert_isequal(ax.Title.FontSizeMode, 'auto');
assert_isapprox(ax.Subtitle.FontSize, 40, 1e-12);
close(f);
%=============================================================================
% 3D: an auto ZLabel is a ruler label.
f = figure('Visible', 'off');
ax = axes(f);
view(ax, 3);
ax.LabelFontSizeMultiplier = 1.5;
ax.FontUnits = 'pixels';
assert_isequal(ax.ZLabel.FontUnits, 'points');
assert_isapprox(ax.ZLabel.FontSize, 15, 1e-12);
close(f);
%=============================================================================
% A label whose FontUnits the user set follows the axes units.
f = figure('Visible', 'off');
ax = axes(f);
ax.XLabel.FontUnits = 'pixels';
assert_isapprox(ax.XLabel.FontSize, 44 / 3, 1e-12);
assert_isequal(ax.XLabel.FontSizeMode, 'auto');
ax.FontSize = 14;
assert_isapprox(ax.XLabel.FontSize, 14 * 1.1 * 4 / 3, 1e-12);
ax.FontUnits = 'inches';
assert_isapprox(ax.FontSize, 14 / 72, 1e-12);
assert_isequal(ax.XLabel.FontUnits, 'inches');
assert_isapprox(ax.XLabel.FontSize, 14 * 1.1 / 72, 1e-12);
assert_isequal(ax.YLabel.FontUnits, 'points');
ax.FontUnits = 'centimeters';
assert_isapprox(ax.FontSize, 14 / 72 * 2.54, 1e-12);
ax.FontUnits = 'points';
assert_isapprox(ax.FontSize, 14, 1e-12);
assert_isequal(ax.XLabel.FontUnits, 'points');
assert_isapprox(ax.XLabel.FontSize, 15.4, 1e-12);
close(f);
%=============================================================================
% xlabel gives the label back points.
f = figure('Visible', 'off');
ax = axes(f);
ax.FontUnits = 'pixels';
ax.FontSize = 20;
h = xlabel(ax, 'x');
assert_isequal(h.FontUnits, 'points');
assert_isapprox(h.FontSize, 16.5, 1e-12);
assert_isequal(h.FontSizeMode, 'auto');
close(f);
%=============================================================================
% Order of FontSize and FontUnits in one set.
f = figure('Visible', 'off');
ax = axes(f);
set(ax, 'FontSize', 20, 'FontUnits', 'pixels');
assert_isapprox(ax.FontSize, 80 / 3, 1e-12);
set(ax, 'FontUnits', 'points', 'FontSize', 12);
assert_isequal(ax.FontSize, 12);
t = text(ax, 0, 0, 'a');
t.FontSize = 12;
t.FontUnits = 'pixels';
assert_isapprox(t.FontSize, 16, 1e-12);
set(t, 'FontUnits', 'points', 'FontSize', 20);
assert_isequal(t.FontSize, 20);
set(t, 'FontSize', 20, 'FontUnits', 'pixels');
assert_isapprox(t.FontSize, 80 / 3, 1e-12);
t.FontUnits = 'inches';
assert_isapprox(t.FontSize, 20 / 72, 1e-12);
t.FontUnits = 'centimeters';
assert_isapprox(t.FontSize, 20 / 72 * 2.54, 1e-12);
t.FontUnits = 'points';
assert_isapprox(t.FontSize, 20, 1e-12);
%=============================================================================
% Normalized font units are relative to the axes height.
ax.Units = 'pixels';
ax.Position(4) = 200;
ax.FontUnits = 'normalized';
assert_isapprox(ax.FontSize, 0.08, 1e-12);
t.FontUnits = 'normalized';
assert_isapprox(t.FontSize, 20 / 150, 1e-12);
ax.FontUnits = 'points';
t.FontUnits = 'points';
assert_isapprox(ax.FontSize, 12, 1e-12);
assert_isapprox(t.FontSize, 20, 1e-12);
close(f);
%=============================================================================
