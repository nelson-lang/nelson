%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [seq, states] = hmmgenerate(len, trans, emis)
  narginchk(3, 3);
  nargoutchk(0, 2);
  if ~(isnumeric(len) && isreal(len) && isscalar(len) && isfinite(len) ...
      && floor(len) == len && len >= 0)
    error('Nelson:statistics:LENNonnegativeIntegerScalar', '%s must be a nonnegative integer scalar.', 'LEN');
  end
  [trans, emis, nstates, ~] = hmmCheckModel(trans, emis);
  len = double(len);
  seq = zeros(1, len);
  states = zeros(1, len);
  if len == 0
    return
  end
  prior = ones(1, nstates) / nstates;
  states(1) = hmmSampleDiscrete(prior);
  seq(1) = hmmSampleDiscrete(emis(states(1), :));
  for t = 2:len
    states(t) = hmmSampleDiscrete(trans(states(t - 1), :));
    seq(t) = hmmSampleDiscrete(emis(states(t), :));
  end
end
%=============================================================================
