%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = sqrtm(a)
  % matrix square root: CPU fallback (a dense matrix factorization), computed on the
  % host and re-wrapped as gpuArray. Results may be complex, which the
  % device also stores.
  r = gpuArray(sqrtm(gather(a)));
end
%=============================================================================
