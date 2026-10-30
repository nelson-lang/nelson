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
pythonAvailable = false;
try
  pyrun('import numpy as np');
  pythonAvailable = true;
catch
end
skip_testsuite(~pythonAvailable, 'NumPy wave demo requires a Python engine.');
%=============================================================================
waveFigureVisible = 'off';
waveAnimationEnabled = true;
waveAnimationFrames = 12;
waveFramePause = 0;
waveAnimationContinuous = false;
run([modulepath('python_engine', 'root'), '/examples/numpy_wave.m']);

assert_istrue(isgraphics(waveFigure));
assert_istrue(isgraphics(waveImage));
% NumPy returned a square field that Nelson converted to a double matrix.
assert_isequal(size(waveField), [waveSize waveSize]);
assert_isequal(class(waveField), 'double');
% The driven sources made the field move away from a flat zero state.
assert_istrue(max(abs(waveField(:))) > 0);
delete(waveFigure);
drawnow();
%=============================================================================
