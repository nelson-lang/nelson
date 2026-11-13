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
% Nested assignments on axes text objects keep their font values.
f = figure('Visible', 'off');
ax = axes(f);
xlabel(ax, 'x');
ylabel(ax, 'y');
zlabel(ax, 'z');
title(ax, 't');
subtitle(ax, 's');
ax.XLabel.FontSize = 17;
ax.YLabel.FontSize = 16;
ax.ZLabel.FontSize = 15;
ax.Title.FontSize = 18;
ax.Subtitle.FontSize = 19;
ax.XLabel.FontWeight = 'bold';
ax.Title.FontWeight = 'normal';
ax.Title.FontAngle = 'italic';
ax.XLabel.FontName = 'Courier';
assert_isequal(ax.XLabel.FontSize, 17);
assert_isequal(ax.YLabel.FontSize, 16);
assert_isequal(ax.ZLabel.FontSize, 15);
assert_isequal(ax.Title.FontSize, 18);
assert_isequal(ax.Subtitle.FontSize, 19);
assert_isequal(ax.XLabel.FontWeight, 'bold');
assert_isequal(ax.Title.FontWeight, 'normal');
assert_isequal(ax.Title.FontAngle, 'italic');
assert_isequal(ax.XLabel.FontName, 'Courier');
%=============================================================================
% Re-assigning the same handle does not resync the fonts.
ax.XLabel = ax.XLabel;
ax.Title = ax.Title;
set(ax, 'Subtitle', ax.Subtitle);
assert_isequal(ax.XLabel.FontSize, 17);
assert_isequal(ax.Title.FontSize, 18);
assert_isequal(ax.Subtitle.FontSize, 19);
assert_isequal(ax.Title.FontWeight, 'normal');
%=============================================================================
% Other forms of the same assignment.
h = ax.YLabel;
h.FontSize = 13;
assert_isequal(ax.YLabel.FontSize, 13);
set(ax.ZLabel, 'FontSize', 12);
assert_isequal(ax.ZLabel.FontSize, 12);
%=============================================================================
% Changing the axes FontSize still resizes all texts.
ax.FontSize = 20;
assert_isequal(ax.XLabel.FontSize, 22);
assert_isequal(ax.YLabel.FontSize, 22);
% The ZLabel of a 2D axes keeps its size.
assert_isequal(ax.ZLabel.FontSize, 12);
assert_isequal(ax.Title.FontSize, 22);
assert_isequal(ax.Subtitle.FontSize, 22);
assert_isequal(ax.XLabel.FontWeight, 'bold');
assert_isequal(ax.Title.FontWeight, 'normal');
%=============================================================================
% title() keeps the user font, xlabel() and ylabel() take back the axes font.
ax.Title.FontSize = 30;
ax.XLabel.FontSize = 25;
ax.XLabel.Color = [1 0 0];
ax.YLabel.FontSize = 24;
title(ax, 'new');
xlabel(ax, 'nx');
ylabel(ax, 'ny');
assert_isequal(ax.Title.FontSize, 30);
assert_isequal(ax.Title.FontWeight, 'normal');
assert_isequal(ax.XLabel.FontSize, 22);
assert_isequal(ax.XLabel.FontWeight, 'normal');
assert_isequal(ax.XLabel.FontName, ax.FontName);
assert_isequal(ax.XLabel.Color, [1 0 0]);
assert_isequal(ax.YLabel.FontSize, 22);
xlabel(ax, 'sized', 'FontSize', 9);
assert_isequal(ax.XLabel.FontSize, 9);
delete(f);
%=============================================================================
% Hidden font modes: a user value turns them manual, even an unchanged one.
f = figure('Visible', 'off');
ax = axes(f);
x = ax.XLabel;
t = ax.Title;
assert_isequal(x.FontSizeMode, 'auto');
assert_isequal(x.FontWeightMode, 'auto');
x.FontWeight = 'normal';
x.FontAngle = 'normal';
x.FontName = 'Courier';
assert_isequal(x.FontSizeMode, 'auto');
assert_isequal(x.FontWeightMode, 'manual');
assert_isequal(x.FontAngleMode, 'manual');
assert_isequal(x.FontNameMode, 'manual');
% Axes font weight, angle and name win over the label values.
ax.FontWeight = 'bold';
ax.FontAngle = 'italic';
ax.FontName = 'Arial';
assert_isequal(x.FontWeight, 'bold');
assert_isequal(x.FontAngle, 'italic');
assert_isequal(x.FontName, 'Arial');
assert_isequal(t.FontName, 'Arial');
% The axes font size wins too and turns the size mode back to auto.
x.FontSize = 17;
assert_isequal(x.FontSizeMode, 'manual');
ax.FontSize = 20;
assert_isequal(x.FontSize, 22);
assert_isequal(x.FontSizeMode, 'auto');
ax.LabelFontSizeMultiplier = 1.5;
assert_isequal(x.FontSize, 30);
% Multipliers and title weights only reach texts in auto mode.
ax.YLabel.FontSize = 12;
ax.LabelFontSizeMultiplier = 2;
assert_isequal(ax.YLabel.FontSize, 12);
assert_isequal(x.FontSize, 40);
t.FontSize = 18;
ax.TitleFontSizeMultiplier = 2;
assert_isequal(t.FontSize, 18);
assert_isequal(ax.Subtitle.FontSize, 40);
t.FontWeight = 'normal';
ax.TitleFontWeight = 'bold';
assert_isequal(t.FontWeight, 'normal');
ax.SubtitleFontWeight = 'bold';
assert_isequal(ax.Subtitle.FontWeight, 'bold');
% An axis color always paints its label; the label keeps its color mode.
x.Color = [0 0 1];
assert_isequal(x.ColorMode, 'manual');
ax.XColor = [0 1 0];
assert_isequal(x.Color, [0 1 0]);
assert_isequal(x.ColorMode, 'manual');
delete(f);
%=============================================================================
% The ZLabel is sized only on a 3D axes.
f = figure('Visible', 'off');
ax = axes(f);
assert_isequal(ax.ZLabel.FontSize, 10);
ax.FontSize = 20;
assert_isequal(ax.ZLabel.FontSize, 10);
view(ax, 3);
assert_isequal(ax.ZLabel.FontSize, 22);
ax.ZLabel.FontSize = 23;
zlabel(ax, 'z');
assert_isequal(ax.ZLabel.FontSize, 22);
delete(f);
%=============================================================================
% A nested assignment through a read-only handle property works.
f = figure('Visible', 'off');
plot(1:3);
l = legend('a');
l.Title.String = 'T';
assert_isequal(l.Title.String, 'T');
delete(f);
%=============================================================================
