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
[previousFigurePosition, previousFigureVisible] = graphics_test_reference_figure_position();
referenceFigureCleanup = onCleanup(@() set(groot(), 'DefaultFigurePosition', previousFigurePosition, 'DefaultFigureVisible', previousFigureVisible)); %#ok<NASGU>
figure();
[X, Y, Z] = sphere(40);
contour3(X, Y, Z, 12);
filename = [tempname(), '.png'];
saveas(gcf(), filename);
ax = gca();
asserts.isequal(ax.XLim, [-1 1]);
asserts.isequal(ax.YLim, [-1 1]);
asserts.isequal(ax.ZLim, [-1 1]);
asserts.isequal(ax.XTick, [-1 -0.5 0 0.5 1]);
asserts.isequal(ax.YTick, [-1 -0.5 0 0.5 1]);
asserts.isequal(ax.ZTick, [-1 -0.5 0 0.5 1]);
rmfile(filename);
%=============================================================================
