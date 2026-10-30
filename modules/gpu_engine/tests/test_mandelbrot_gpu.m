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
mandelbrotFigureVisible = 'off';
mandelbrotAnimationEnabled = true;
mandelbrotAnimationFrames = 3;
mandelbrotFramePause = 0;
mandelbrotAnimationContinuous = false;
mandelbrotResolution = 80;
run([modulepath('gpu_engine', 'root'), '/examples/mandelbrot_gpu.m']);

assert_istrue(isgraphics(mandelbrotFigure));
assert_istrue(isgraphics(mandelbrotImage));
cdata = get(mandelbrotImage, 'CData');
assert_isequal(size(cdata), [80 80 3]);
assert_istrue(all(cdata(:) >= 0 & cdata(:) <= 1));
% The demo reports whether it ran on the GPU or fell back to the CPU.
assert_istrue(strcmp(mandelbrotDeviceLabel, 'GPU') || ...
  strcmp(mandelbrotDeviceLabel, 'CPU'));
delete(mandelbrotFigure);
drawnow();
%=============================================================================
