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
chirpSpectrogramFigureVisible = 'off';
run([modulepath('signal_processing', 'root'), '/examples/chirp_spectrogram.m']);

assert(length(sampleTimes) == 2000);
assert(isequal(size(spectrogramValues), size(powerValues)));
assert(all(isfinite(powerValues), 'all'));
assert(ridgeFrequencies(end) > ridgeFrequencies(1) + 200);
assert(isgraphics(figureHandle));
close(figureHandle);
%=============================================================================
