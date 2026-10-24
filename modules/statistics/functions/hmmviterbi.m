%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [states, logp] = hmmviterbi(seq, trans, emis)
  narginchk(3, 3);
  nargoutchk(0, 2);
  [trans, emis, nstates, nsymbols] = __hmm_check_model(trans, emis);
  seq = __hmm_check_sequence(seq, nsymbols, 'SEQ');
  tiny = realmin;
  len = length(seq);
  delta = zeros(len, nstates);
  psi = zeros(len, nstates);
  delta(1, :) = log(ones(1, nstates) / nstates) + log(max(emis(:, seq(1))', tiny));
  for t = 2:len
    for j = 1:nstates
      [value, idx] = max(delta(t - 1, :) + log(max(trans(:, j)', tiny)));
      delta(t, j) = value + log(max(emis(j, seq(t)), tiny));
      psi(t, j) = idx;
    end
  end
  [logp, last] = max(delta(len, :));
  states = zeros(1, len);
  states(len) = last;
  for t = len - 1:-1:1
    states(t) = psi(t + 1, states(t + 1));
  end
end
%=============================================================================
