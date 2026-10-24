%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function z = optimFminuncLBFGSMultiply(memory, g)
  q = g(:);
  alpha = zeros(numel(memory), 1);
  for k = numel(memory):-1:1
    alpha(k) = memory{k}.rho * (memory{k}.s' * q);
    q = q - alpha(k) * memory{k}.y;
  end
  z = scaleInitial(memory, q);
  for k = 1:numel(memory)
    beta = memory{k}.rho * (memory{k}.y' * z);
    z = z + memory{k}.s * (alpha(k) - beta);
  end
end
%=============================================================================
function z = scaleInitial(memory, q)
  if isempty(memory)
    z = q;
    return
  end
  last = memory{end};
  gamma = (last.s' * last.y) / max(last.y' * last.y, eps);
  z = gamma * q;
end
%=============================================================================
