%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
ls(nelsonroot())
l = ls(nelsonroot());
assert_istrue(ischar(l));
if ispc()
  assert_istrue(size(l, 1) > 5);
else
  R = find(l == char(10));
  assert_istrue(numel(R) > 5);
end
%=============================================================================
