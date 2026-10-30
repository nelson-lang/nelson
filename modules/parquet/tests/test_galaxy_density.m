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
galaxyFigureVisible = 'off';
galaxyAnimationEnabled = true;
galaxyFramePause = 0;
galaxyAnimationContinuous = false;
galaxyStarCount = 8000;
run([modulepath('parquet', 'root'), '/examples/galaxy_density.m']);

assert_istrue(isgraphics(galaxyFigure));
% The stars were written to and read back from a Parquet file.
assert_istrue(isfile(galaxyFile));
assert_isequal(numel(galaxyX), galaxyStarCount);
% Every star was binned into the density grid.
assert_isequal(galaxyProcessed, galaxyStarCount);
assert_isequal(sum(galaxyDensity(:)), galaxyStarCount);
delete(galaxyFigure);
drawnow();
%=============================================================================
