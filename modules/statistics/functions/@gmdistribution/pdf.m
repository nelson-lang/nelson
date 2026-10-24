%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = pdf(obj, X)
  narginchk(2, 2);
  nargoutchk(0, 1);
  [weighted, ~] = __gmm_weighted_density__(X, obj.mu, obj.Sigma, obj.ComponentProportion);
  y = weighted * ones(size(weighted, 2), 1);
end
%=============================================================================
