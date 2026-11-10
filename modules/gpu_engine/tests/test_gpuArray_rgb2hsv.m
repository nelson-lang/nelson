%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(~canUseGPU(), 'No compatible GPU device found.');
%=============================================================================
% colormaps and images give the host result, ties between channels included
map = single([0.2 0.5 0.7; 0 0 0; 1 1 1; 0.5 0.5 0.2; 0.3 0.3 0.6; 0.9 0.1 0.9; 0.4 0.8 0.4]);
R = rgb2hsv(gpuArray(map));
assert_isequal(class(R), 'gpuArray');
assert_isequal(size(R), [7 3]);
assert_istrue(max(abs(gather(R(:)) - rgb2hsv(reshape(map, 7, 1, 3))(:))) < 1e-6);
img = single(rand(4, 5, 3));
R = rgb2hsv(gpuArray(img));
assert_isequal(size(R), [4 5 3]);
assert_istrue(max(abs(gather(R(:)) - rgb2hsv(img)(:))) < 1e-6);
%=============================================================================
% integer images run on the host, colormap values outside [0, 1] are rejected
R = rgb2hsv(gpuArray(uint8(cat(3, [0 255], [128 0], [255 64]))));
assert_isequal(size(R), [1 2 3]);
msg = _('Colormap must be a double matrix with values in the range [0, 1].');
assert_checkerror('rgb2hsv(gpuArray([0.2 1.5 0.3]))', msg, 'Nelson:image_processing:colormapDoubleMatrixValuesRange');
%=============================================================================
