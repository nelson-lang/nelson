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
% colormap lookup on the device gives the host result
map = repmat(linspace(0, 1, 4)', 1, 3);
cases = {[1 2; 3 4], [0 5 NaN 2], single([1 4]), uint8([0 1 3 9]), int8([0 2]), zeros(0, 3)};
for k = 1:numel(cases)
  x = cases{k};
  R = ind2rgb(gpuArray(x), map);
  assert_isequal(class(R), 'gpuArray');
  expected = ind2rgb(x, map);
  assert_isequal(size(R), size(expected));
  assert_istrue(isempty(expected) || max(abs(double(gather(R(:))) - expected(:))) < 1e-6);
end
R = ind2rgb(gpuArray([1 2]), gpuArray(map));
assert_istrue(max(abs(double(gather(R(:))) - reshape(ind2rgb([1 2], map), [], 1))) < 1e-6);
%=============================================================================
% invalid indices keep the host errors
msg = _('Indexed image values must be finite integers.');
id = 'Nelson:image_processing:indexedImageValuesFiniteIntegers';
assert_checkerror('ind2rgb(gpuArray([1.5 2]), eye(3))', msg, id);
msg = _('Colormap must have at least three columns.');
assert_checkerror('ind2rgb(gpuArray([1 2]), [0 1; 1 0])', msg, 'Nelson:image_processing:colormapLeastThreeColumns');
%=============================================================================
