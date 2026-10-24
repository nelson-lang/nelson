%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [P, nlogL] = posterior(obj, X)
  narginchk(2, 2);
  nargoutchk(0, 2);
  [P, nlogL] = __gmm_responsibilities__(X, obj.mu, obj.Sigma, obj.ComponentProportion);
end
%=============================================================================
