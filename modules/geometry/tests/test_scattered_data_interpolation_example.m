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
scatteredDataFigureVisible = 'off';
run([modulepath('geometry', 'root'), '/examples/scattered_data_interpolation.m']);

assert(isequal(size(samplePoints), [25 2]));
assert(size(triangles, 2) == 3);
assert(~isempty(voronoiX) && isequal(size(voronoiX), size(voronoiY)));
assert(isequal(size(interpolatedValues), [45 45]));
assert(all(isfinite(interpolatedValues), 'all'));
assert(isgraphics(figureHandle));
close(figureHandle);
%=============================================================================
