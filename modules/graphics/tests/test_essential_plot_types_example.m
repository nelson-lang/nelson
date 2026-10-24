%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
essentialPlotsFigureVisible = 'off';
run([modulepath('graphics', 'root'), '/examples/essential_plot_types.m']);

assert(isgraphics(figureHandle));
assert(isgraphics(lineAxes) && isgraphics(errorAxes));
assert(isgraphics(barAxes) && isgraphics(histogramAxes));
assert(isgraphics(contourAxes) && isgraphics(surfaceAxes));
assert(isequal(size(surfaceZ), [45 45]));
close(figureHandle);
%=============================================================================
