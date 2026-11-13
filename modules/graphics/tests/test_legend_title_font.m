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
% A user-set legend title FontSize sticks.
f = figure('Visible', 'off');
ax = axes(f);
plot(ax, 1:3);
l = legend(ax, 'a');
h = l.Title;
h.String = 'T';
h.FontSize = 15;
drawnow();
assert_isequal(h.FontSize, 15);
assert_isequal(h.FontSizeMode, 'manual');
l.FontSize = 12;
drawnow();
assert_isequal(h.FontSize, 15);
ax.FontSize = 20;
drawnow();
assert_isequal(l.FontSize, 12);
assert_isequal(h.FontSize, 15);
l.FontSizeMode = 'auto';
drawnow();
assert_isapprox(l.FontSize, 18, 1e-12);
assert_isequal(h.FontSize, 15);
%=============================================================================
% Back to auto, the title follows the legend again.
h.FontSizeMode = 'auto';
l.FontSize = 11;
drawnow();
assert_isequal(h.FontSize, 11);
ax.FontSize = 10;
drawnow();
assert_isequal(l.FontSize, 11);
assert_isequal(h.FontSize, 11);
%=============================================================================
% Nested assignment form.
l.Title.FontSize = 7;
drawnow();
assert_isequal(l.Title.FontSize, 7);
l.FontSize = 13;
drawnow();
assert_isequal(l.Title.FontSize, 7);
%=============================================================================
% An auto title follows the legend FontSize and the axes FontSize.
f2 = figure('Visible', 'off');
ax2 = axes(f2);
plot(ax2, 1:3);
l2 = legend(ax2, 'a');
h2 = l2.Title;
drawnow();
assert_isapprox(h2.FontSize, 9, 1e-12);
assert_isequal(h2.FontSizeMode, 'auto');
l2.FontSize = 12;
drawnow();
assert_isequal(h2.FontSize, 12);
assert_isequal(h2.FontSizeMode, 'auto');
l2.FontSizeMode = 'auto';
ax2.FontSize = 20;
drawnow();
assert_isapprox(l2.FontSize, 18, 1e-12);
assert_isapprox(h2.FontSize, 18, 1e-12);
h2.FontSize = 8;
ax2.FontSize = 30;
drawnow();
assert_isapprox(l2.FontSize, 27, 1e-12);
assert_isequal(h2.FontSize, 8);
%=============================================================================
% Legend font attributes reach an auto title and keep its modes auto.
l2.FontWeight = 'bold';
l2.FontAngle = 'italic';
l2.FontName = 'Courier New';
l2.TextColor = [1 0 0];
drawnow();
assert_isequal(h2.FontWeight, 'bold');
assert_isequal(h2.FontWeightMode, 'auto');
assert_isequal(h2.FontAngle, 'italic');
assert_isequal(h2.FontAngleMode, 'auto');
assert_isequal(h2.FontName, 'Courier New');
assert_isequal(h2.FontNameMode, 'auto');
assert_isequal(h2.Color, [1 0 0]);
%=============================================================================
% A user value equal to the current one still turns manual.
f3 = figure('Visible', 'off');
ax3 = axes(f3);
plot(ax3, 1:3);
l3 = legend(ax3, 'a');
h3 = l3.Title;
drawnow();
h3.FontSize = h3.FontSize;
assert_isequal(h3.FontSizeMode, 'manual');
h3.FontWeight = 'normal';
assert_isequal(h3.FontWeightMode, 'manual');
l3.FontSize = 13;
l3.FontWeight = 'bold';
drawnow();
assert_isapprox(h3.FontSize, 9, 1e-12);
assert_isequal(h3.FontWeight, 'normal');
%=============================================================================
% A legend FontSize given at creation reaches the title.
f4 = figure('Visible', 'off');
ax4 = axes(f4);
plot(ax4, 1:3);
l4 = legend(ax4, 'a', 'FontSize', 14);
h4 = l4.Title;
drawnow();
assert_isequal(h4.FontSize, 14);
%=============================================================================
close all;
%=============================================================================
