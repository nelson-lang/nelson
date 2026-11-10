%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Y = pagetranspose(X)
  % transpose of every page: the first two dimensions swap on the device
  Y = permute(X, [2 1 3:max(2, ndims(X))]);
end
%=============================================================================
