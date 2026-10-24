%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function problem = prob2struct(prob, varargin)
  if ~isa(prob, 'optim.problemdef.OptimizationProblem')
    error('Nelson:optimization:optimizationProblemExpected', 'Optimization problem expected.');
  end
  problem = prob.prob2struct(varargin{:});
end
%=============================================================================
