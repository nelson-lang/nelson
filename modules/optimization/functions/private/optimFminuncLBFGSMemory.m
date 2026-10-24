%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function m = optimFminuncLBFGSMemory(opts)
  value = opts.HessianApproximation;
  if iscell(value) && numel(value) >= 2
    m = max(1, round(double(value{2})));
  else
    m = 10;
  end
end
%=============================================================================
