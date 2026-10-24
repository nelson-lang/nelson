%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = nanvar(x, varargin)
  narginchk(1, 3);
  nargoutchk(0, 1);
  [weights, args] = parseNanvarInputsLocal(varargin);
  y = __nan_statistics__(x, 'var', weights, args{:});
end
%=============================================================================
function [weights, args] = parseNanvarInputsLocal(args)
  weights = 0;
  if isempty(args)
    return
  end
  weights = args{1};
  if isempty(weights)
    weights = 0;
  end
  if ~(isnumeric(weights) && isreal(weights) && (isscalar(weights) || isvector(weights)))
    error('Nelson:statistics:weightsScalarVector', 'Weights must be a scalar or vector.');
  end
  if isscalar(weights) && ~(weights == 0 || weights == 1)
    error('Nelson:statistics:weights01NonnegativeVector', 'Weights must be 0, 1, or a nonnegative vector.');
  end
  if ~isscalar(weights) && any(weights < 0)
    error('Nelson:statistics:weights01NonnegativeVector', 'Weights must be 0, 1, or a nonnegative vector.');
  end
  args = args(2:end);
end
%=============================================================================
