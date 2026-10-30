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
newtonFractalFigureVisible = 'off';
newtonFractalAnimationEnabled = true;
newtonFractalAnimationFrames = 4;
newtonFractalFramePause = 0;
newtonFractalAnimationContinuous = false;
newtonFractalResolution = 60;
% Stop at a moderate zoom so the final frame still shows all three basins.
newtonFractalEndSpan = 0.6;
run([modulepath('polynomial_functions', 'root'), ...
  '/examples/newton_fractal.m']);

assert_isequal(numel(newtonFractalRoots), 3);
assert_istrue(isgraphics(newtonFractalFigure));
assert_istrue(isgraphics(newtonFractalImage));
cdata = get(newtonFractalImage, 'CData');
assert_isequal(size(cdata), [60 60 3]);
assert_istrue(all(cdata(:) >= 0));
assert_istrue(all(cdata(:) <= 1));
assert_istrue(max(cdata(:)) > 0.5);
% All three basins must appear, so the coloured image is not monochrome.
channelSpread = max(cdata, [], [1 2]) - min(cdata, [], [1 2]);
assert_istrue(all(channelSpread(:) > 0.1));
delete(newtonFractalFigure);
drawnow();
%=============================================================================
