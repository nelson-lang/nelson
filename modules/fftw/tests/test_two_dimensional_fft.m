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
fftExampleVisible = 'off';
run([modulepath('fftw', 'root'), '/examples/two_dimensional_fft.m']);
assert(isequal(size(centeredTransform), [128 128]));
assert(logMagnitude(65, 65) == max(logMagnitude(:)));
assert(strcmp(get(fftExampleFigure, 'Visible'), 'off'));
close(fftExampleFigure);
%=============================================================================
