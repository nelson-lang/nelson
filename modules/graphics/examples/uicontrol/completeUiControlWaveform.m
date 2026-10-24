%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [y, waveformLabel] = completeUiControlWaveform(...
  state, amplitude, frequency, phase)
  argument = frequency * state.x + phase;
  if state.sineRadio.Value
    y = amplitude * sin(argument);
    waveformLabel = 'sine';
  else
    y = amplitude * cos(argument);
    waveformLabel = 'cosine';
  end
end
%=============================================================================
