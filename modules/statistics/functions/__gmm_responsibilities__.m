%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [P, nlogL, y] = __gmm_responsibilities__(X, mu, Sigma, p)
  [weighted, ~] = __gmm_weighted_density__(X, mu, Sigma, p);
  y = weighted * ones(size(weighted, 2), 1);
  safe = max(y, realmin);
  P = weighted ./ repmat(safe, 1, size(weighted, 2));
  nlogL = -sum(log(safe));
end
%=============================================================================
