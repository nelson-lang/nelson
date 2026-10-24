%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = mrdivide(a, b)
  % CPU fallback: computed on host, re-wrapped as gpuArray.
  r = gpuArray(gather(a) / gather(b));
end
%=============================================================================
