%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sol = dde23(ddefun, lags, history, tspan, options)
  narginchk(4, 5);
  if nargin < 5
    options = [];
  end
  sol = ddeRunSolver('dde23', ddefun, lags, [], history, tspan, options);
end
%=============================================================================
