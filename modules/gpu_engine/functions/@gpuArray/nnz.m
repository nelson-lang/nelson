%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function n = nnz(x)
  % number of nonzero elements: counted on the device, returned as a host double.
  n = double(gather(sum(x(:) ~= 0)));
end
%=============================================================================
