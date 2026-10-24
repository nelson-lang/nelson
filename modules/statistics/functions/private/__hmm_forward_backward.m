%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [gamma, xiSum, logp, alpha, beta, scale] = __hmm_forward_backward(seq, trans, emis)
  nstates = size(trans, 1);
  len = length(seq);
  alpha = zeros(len, nstates);
  beta = zeros(len, nstates);
  scale = zeros(len, 1);
  prior = ones(1, nstates) / nstates;
  alpha(1, :) = prior .* emis(:, seq(1))';
  [alpha(1, :), scale(1)] = normalizeRow(alpha(1, :));
  for t = 2:len
    alpha(t, :) = (alpha(t - 1, :) * trans) .* emis(:, seq(t))';
    [alpha(t, :), scale(t)] = normalizeRow(alpha(t, :));
  end
  beta(len, :) = ones(1, nstates);
  for t = len - 1:-1:1
    beta(t, :) = (trans * (emis(:, seq(t + 1)) .* beta(t + 1, :)'))' / scale(t + 1);
  end
  gamma = alpha .* beta;
  gamma = gamma ./ repmat(sum(gamma, 2), 1, nstates);
  xiSum = zeros(nstates, nstates);
  for t = 1:len - 1
    xi = trans .* (alpha(t, :)' * (emis(:, seq(t + 1))' .* beta(t + 1, :)));
    total = sum(xi(:));
    if total > 0
      xiSum = xiSum + xi / total;
    end
  end
  logp = sum(log(scale));
end
%=============================================================================
function [row, scale] = normalizeRow(row)
  scale = sum(row);
  if scale <= 0
    row(:) = 1 / length(row);
    scale = realmin;
  else
    row = row / scale;
  end
end
%=============================================================================
