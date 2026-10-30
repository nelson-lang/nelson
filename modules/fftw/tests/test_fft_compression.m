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
fftCompFigureVisible = 'off';
fftCompAnimationEnabled = true;
fftCompAnimationFrames = 3;
fftCompFramePause = 0;
fftCompAnimationContinuous = false;
fftCompSize = 64;
% Keep every coefficient so the reconstruction must equal the original.
fftCompStartFraction = 1.0;
fftCompEndFraction = 1.0;
run([modulepath('fftw', 'root'), '/examples/fft_compression.m']);

assert_istrue(isgraphics(fftCompFigure));
assert_istrue(isgraphics(fftCompReconImage));
reconstruction = get(fftCompReconImage, 'CData');
assert_isequal(size(reconstruction), [fftCompSize fftCompSize 3]);
% With all coefficients kept, ifft2(fft2(x)) rebuilds the image exactly.
reconstructedGray = reconstruction(:, :, 1);
assert_istrue(max(abs(reconstructedGray(:) - fftCompImage(:))) < 1e-6);
delete(fftCompFigure);
drawnow();
%=============================================================================
