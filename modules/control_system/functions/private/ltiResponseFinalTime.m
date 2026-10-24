%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tFinal = ltiResponseFinalTime(sys)
  % Automatic final time for a time response plot, derived from the
  % dominant (slowest) stable pole of the model. Returns [] when no
  % settling horizon can be determined (no strictly stable pole), so
  % callers fall back to a fixed default horizon.
  tFinal = [];
  p = pole(sys);
  if isempty(p)
    return;
  end
  Ts = sys.Ts;
  if (Ts > 0)
    % Map the discrete poles to their continuous s-plane equivalent so the
    % settling estimate below is expressed in the same (physical) time unit.
    p = p(abs(p) > 0);
    if isempty(p)
      return;
    end
    s = log(p) / Ts;
  else
    s = p;
  end
  decay = -real(s); % decay rate of each mode
  decay = decay(decay > sqrt(eps)); % keep strictly stable modes only
  if isempty(decay)
    return;
  end
  slowest = min(decay);
  % Settling window of the dominant first-order mode: the response of a mode
  % with decay rate 'slowest' has decayed to a negligible residual after this
  % many time constants, which sets a sensible automatic simulation horizon.
  settlingFactor = 5.8025144343;
  tFinal = settlingFactor / slowest;
end
%=============================================================================
