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
reset(groot());
[previousFigurePosition, previousFigureVisible] = graphics_test_reference_figure_position();
referenceFigureCleanup = onCleanup(@() set(groot(), 'DefaultFigurePosition', previousFigurePosition, 'DefaultFigureVisible', previousFigureVisible)); %#ok<NASGU>
figure();
t = linspace(0, 10 * pi, 400);
x = sin(t);
y = cos(t);
z = t;
R = line(x, y, z);
view(3)
filename = [tempname(), '.png'];
saveas(gcf(), filename);
ax = gca();
asserts.isequal(ax.XLim, [-1 1]);
asserts.isequal(ax.YLim, [-1 1]);
asserts.isequal(ax.ZLim(1), 0);
asserts.istrue(ax.ZLim(2) >= max(z));
asserts.istrue(ax.ZLim(2) <= 40);
asserts.isequal(ax.XTick, [-1 -0.5 0 0.5 1]);
asserts.isequal(ax.YTick, [-1 -0.5 0 0.5 1]);
asserts.isequal(ax.ZTick, 0:5:ax.ZLim(2));
rmfile(filename);
close all;
%=============================================================================
