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
heatFigureVisible = 'off';
heatAnimationEnabled = true;
heatAnimationFrames = 60;
heatFramePause = 0;
heatAnimationContinuous = false;
heatGridSize = 60;
run([modulepath('sparse', 'root'), '/examples/heat_diffusion.m']);

assert_istrue(isgraphics(heatFigure));
assert_istrue(issparse(heatLaplacian));
assert_isequal(size(heatLaplacian), [heatGridSize * heatGridSize ...
  heatGridSize * heatGridSize]);
% The field stays bounded by the injected source temperature.
assert_istrue(max(heatField) <= 1 + 1e-9);
assert_istrue(min(heatField) >= -1e-9);
% Heat has actually spread from the sources into the plate.
assert_istrue(sum(heatField > 0.01) > 10);
delete(heatFigure);
drawnow();
%=============================================================================
