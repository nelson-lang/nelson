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
% isequal and isequaln compare on the device and answer on the host
assert_istrue(isequal(gpuArray([1 2]), [1 2]));
assert_isfalse(isequal(gpuArray([1 2]), gpuArray([1 2]), [1 3]));
assert_isfalse(isequal(gpuArray([1 2]), [1 2]'));
assert_isfalse(isequal(gpuArray([1 NaN]), [1 NaN]));
assert_istrue(isequaln(gpuArray([1 NaN]), [1 NaN]));
assert_isfalse(isequaln(gpuArray([1 NaN]), [NaN 1]));
assert_istrue(isequal(gpuArray(true), 1));
assert_istrue(isequal(gpuArray([]), []));
assert_isequal(class(isequal(gpuArray(1), 1)), 'logical');
% text, integer and cell inputs keep the host comparison
assert_istrue(isequal(gpuArray([97 98]), 'ab'));
assert_istrue(isequal(gpuArray(int8([1 2])), int8([1 2])));
assert_isfalse(isequal(gpuArray(1), {1}));
assert_checkerror('isequal(gpuArray(1))', _('Not enough input arguments.'));
%=============================================================================
% allfinite and uplus
assert_isfalse(allfinite(gpuArray([1 Inf])));
assert_istrue(allfinite(gpuArray(single([1 2]))));
assert_isequal(class(allfinite(gpuArray(1))), 'logical');
R = uplus(gpuArray(int8([1 -2])));
assert_isequal(class(R), 'gpuArray');
assert_isequal(gather(R), int8([1 -2]));
%=============================================================================
% rgb2gray converts float colormaps and images on the device
map = single([0.2 0.5 0.7; 0 0 0; 1 1 1; 0.4 0.8 0.4]);
R = rgb2gray(gpuArray(map));
assert_isequal(size(R), [4 3]);
expected = rgb2gray(reshape(map, 4, 1, 3));
assert_istrue(max(abs(gather(R(:, 1)) - expected(:))) < 1e-6);
img = single(rand(3, 4, 3));
R = rgb2gray(gpuArray(img));
assert_isequal(size(R), [3 4]);
assert_istrue(max(abs(gather(R(:)) - rgb2gray(img)(:))) < 1e-6);
R = rgb2gray(gpuArray(uint8(cat(3, [10 200], [20 100], [30 0]))));
assert_isequal(gather(R), rgb2gray(uint8(cat(3, [10 200], [20 100], [30 0]))));
%=============================================================================
