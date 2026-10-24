%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function env = synth_piano_envelope(t, params)
  % ADSR level at times t (seconds, column): linear attack, exponential decay
  % towards the sustain level, held for params.hold, then exponential release.
  % Shared by the sound engine and the envelope display, so the curve on
  % screen is exactly the one applied to the sound.
  a = max(params.attack, 1e-3);
  d = max(params.decay, 1e-3);
  s = params.sustain;
  held = params.attack + params.hold;
  env = zeros(size(t));
  rising = t < a;
  env(rising) = t(rising) / a;
  decaying = ~rising;
  env(decaying) = s + (1 - s) * exp(-(t(decaying) - a) / (d / 4));
  releasing = t >= held;
  levelAtRelease = s + (1 - s) * exp(-max(0, held - a) / (d / 4));
  env(releasing) = levelAtRelease * exp(-(t(releasing) - held) / (max(params.release, 1e-3) / 5));
end
%=============================================================================
