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
% im2double scales integer images on the device like the host
cases = {uint8([0 128 255]), uint16([0 1000 65535]), int16([-32768 0 32767]), true(1, 2), single([0.5 2])};
for k = 1:numel(cases)
  R = im2double(gpuArray(cases{k}));
  assert_isequal(class(R), 'gpuArray');
  assert_istrue(max(abs(double(gather(R)) - im2double(cases{k}))) < 1e-6);
end
R = im2double(gpuArray(uint8([0 2])), 'indexed');
assert_isequal(double(gather(R)), [1 3]);
msg = _('Image must be double, logical, uint8, uint16, int16, or single.');
assert_checkerror('im2double(gpuArray(int8(1)))', msg, 'Nelson:image_processing:imageDoubleLogicalUint8Uint16Int16');
%=============================================================================
% full keeps a gpuArray on the device
R = full(gpuArray([1 2]));
assert_isequal(class(R), 'gpuArray');
assert_isequal(double(gather(R)), [1 2]);
%=============================================================================
