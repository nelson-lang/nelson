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
% colormaps and images give the host result on the device
map = single([0 1 1; 0.1 0.5 0.7; 0.3 1 0.2; 0.5 0.5 0.5; 0.7 0.3 0.9; 0.95 1 1; 1 0 0.4]);
R = hsv2rgb(gpuArray(map));
assert_isequal(class(R), 'gpuArray');
assert_isequal(size(R), [7 3]);
assert_istrue(max(abs(gather(R(:)) - hsv2rgb(map)(:))) < 1e-6);
img = single(rand(4, 5, 3));
R = hsv2rgb(gpuArray(img));
assert_isequal(size(R), [4 5 3]);
assert_istrue(max(abs(gather(R(:)) - hsv2rgb(img)(:))) < 1e-6);
% round trip with rgb2hsv
assert_istrue(max(abs(gather(hsv2rgb(rgb2hsv(gpuArray(img)))) - img)(:)) < 1e-5);
%=============================================================================
% invalid hues and out of range colormaps use the host rules
img(1, 1, 1) = NaN;
R = gather(hsv2rgb(gpuArray(img)));
assert_istrue(all(isnan(R(1, 1, :))));
msg = _('HSV colormap values must be in the range [0, 1].');
assert_checkerror('hsv2rgb(gpuArray([0.2 1.5 0.3]))', msg, 'Nelson:image_processing:hsvColormapValuesRange');
%=============================================================================
