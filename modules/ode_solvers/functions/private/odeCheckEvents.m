%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [eventState, stop, ev] = odeCheckEvents(problem, eventState, t0, y0, t1, y1)
  stop = false;
  ev = odeEmptyEvent(y0);
  if isempty(problem.options.Events)
    return
  end
  [value, isterminal, direction] = odeCallEvents(problem, t1, y1);
  value = value(:);
  isterminal = isterminal(:);
  direction = direction(:);
  for k = 1:length(value)
    isInitialHit = eventState.isInitial(k) && eventState.value(k) == 0 ...
      && odeDirectionMatches(value(k) - eventState.value(k), direction(k));
    if isInitialHit
      [callbackStop, eventY] = odeApplyEventCallback(problem, t0, y0, k);
      ev.t(end + 1) = t0;
      ev.y(:, end + 1) = eventY;
      ev.i(end + 1) = k;
      stop = stop || callbackStop;
    elseif odeHasCrossing(eventState.value(k), value(k), direction(k))
      [eventT, eventY] = odeLocateEvent(problem, k, eventState.value(k), value(k), t0, y0, t1, y1);
      [callbackStop, eventY] = odeApplyEventCallback(problem, eventT, eventY, k);
      ev.t(end + 1) = eventT;
      ev.y(:, end + 1) = eventY;
      ev.i(end + 1) = k;
      stop = stop || callbackStop || logical(isterminal(k));
    end
  end
  eventState.value = value;
  eventState.isterminal = isterminal;
  eventState.direction = direction;
  eventState.isInitial = odeLogicalLike(value, false);
end

function tf = odeDirectionMatches(slope, direction)
  tf = direction == 0 || (direction > 0 && slope > 0) || (direction < 0 && slope < 0);
end
%=============================================================================
function [eventT, eventY] = odeLocateEvent(problem, index, v0, v1, t0, y0, t1, y1)
  leftAlpha = 0;
  rightAlpha = 1;
  leftValue = v0;
  rightValue = v1;
  if rightValue == 0
    eventT = t1;
    eventY = y1;
    return
  end
  if leftValue == 0
    eventT = t0;
    eventY = y0;
    return
  end
  % Hermite interpolation of the state inside the step (using the end-point
  % slopes) so the located event tracks the solution curve, not its chord.
  slopes = [odeRhs(problem, t0, y0), odeRhs(problem, t1, y1)];
  for iter = 1:40
    midAlpha = (leftAlpha + rightAlpha) / 2;
    midT = t0 + midAlpha * (t1 - t0);
    midY = odeLinearInterpolate([t0 t1], [y0 y1], midT, slopes);
    values = odeCallEvents(problem, midT, midY);
    midValue = values(index);
    if midValue == 0
      leftAlpha = midAlpha;
      rightAlpha = midAlpha;
      break
    end
    if sign(leftValue) == sign(midValue)
      leftAlpha = midAlpha;
      leftValue = midValue;
    else
      rightAlpha = midAlpha;
      rightValue = midValue;
    end
  end
  alpha = (leftAlpha + rightAlpha) / 2;
  eventT = t0 + alpha * (t1 - t0);
  eventY = odeLinearInterpolate([t0 t1], [y0 y1], eventT, slopes);
end
%=============================================================================
