%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = mape(F, A, varargin)
  % mean absolute percentage error of F against the actual values A, in percent
  narginchk(2, inf);
  r = 100 * errorMetric(F, A, true, varargin{:});
end
%=============================================================================
