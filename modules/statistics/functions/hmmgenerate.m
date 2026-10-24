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
  [trans, emis, nstates, ~] = __hmm_check_model(trans, emis);
  len = double(len);
  seq = zeros(1, len);
  states = zeros(1, len);
  if len == 0
    return
  end
  prior = ones(1, nstates) / nstates;
  states(1) = __hmm_sample_discrete(prior);
  seq(1) = __hmm_sample_discrete(emis(states(1), :));
  for t = 2:len
    states(t) = __hmm_sample_discrete(trans(states(t - 1), :));
    seq(t) = __hmm_sample_discrete(emis(states(t), :));
  end
end
%=============================================================================
