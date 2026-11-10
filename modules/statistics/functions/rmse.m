%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = rmse(F, A, varargin)
  % root mean square error between F and A along a dimension
  narginchk(2, inf);
  r = sqrt(errorMetric(F, A, false, varargin{:}));
end
%=============================================================================
