%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Animate a cellular automaton on host arrays or device arrays.
if exist('figure') == 0
  fprintf('This example requires a graphical Nelson session.\n');
  return
end

rng(42);
gridSize = 140;
frameCount = 160;
liveDensity = 0.24;

initialState = rand(gridSize) > (1 - liveDensity);

if ~canUseGPU()
  state = initialState;
  engineLabel = 'CPU fallback';
  fprintf('No compatible GPU device found; animating on host arrays.\n');
else
  state = gpuArray(initialState);
  engineLabel = 'GPU';
end

figureHandle = figure('Name', 'GPU cellular automaton', 'NumberTitle', 'off');
axesHandle = axes('Parent', figureHandle);
frame = gather(state);
imageHandle = image('Parent', axesHandle, 'CData', double(frame), ...
  'CDataMapping', 'scaled');
axis(axesHandle, 'image');
axis(axesHandle, 'off');
colormap(axesHandle, [0.02 0.04 0.08; 0.15 0.85 0.55]);

for frameIndex = 1:frameCount
  if ~isgraphics(figureHandle) || ~isgraphics(imageHandle)
    break
  end
  state = localLifeStep(state);
  frame = gather(state);
  set(imageHandle, 'CData', double(frame));
  title(axesHandle, sprintf('%s - generation %d - live cells %d', ...
    engineLabel, frameIndex, nnz(frame)));
  drawnow();
  pause(0.025);
end
%=============================================================================
function state = localLifeStep(state)
  neighbors = localShift(state, -1, -1) + localShift(state, -1, 0) ...
    + localShift(state, -1, 1) + localShift(state, 0, -1) ...
    + localShift(state, 0, 1) + localShift(state, 1, -1) ...
    + localShift(state, 1, 0) + localShift(state, 1, 1);
  state = (neighbors == 3) | (state & (neighbors == 2));
end
%=============================================================================
function shifted = localShift(state, rowShift, columnShift)
  shifted = state;
  if rowShift ~= 0
    shifted = circshift(shifted, rowShift, 1);
  end
  if columnShift ~= 0
    shifted = circshift(shifted, columnShift, 2);
  end
end
%=============================================================================
