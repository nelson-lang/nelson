%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% canUseGPU never errors, even without any GPU device
r = canUseGPU();
assert_istrue(islogical(r));
assert_istrue(isscalar(r));
%=============================================================================
% gpuDeviceCount is coherent with canUseGPU
n = gpuDeviceCount();
assert_istrue(isnumeric(n));
if r
  assert_istrue(n >= 1);
else
  assert_isequal(n, 0);
end
%=============================================================================
