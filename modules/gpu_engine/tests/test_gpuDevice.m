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
d = gpuDevice();
assert_istrue(isstruct(d));
assert_isequal(d.Index, 1);
assert_istrue(ischar(d.Name));
assert_istrue(ischar(d.Backend));
assert_istrue(d.MaxBufferSize > 0);
if ismac()
  assert_isequal(d.Backend, 'Metal');
end
%=============================================================================
% invalid device index
assert_checkerror('gpuDevice(2)', _('Invalid GPU device index.'));
%=============================================================================
