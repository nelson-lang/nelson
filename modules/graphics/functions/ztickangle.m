%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = ztickangle(varargin)
  narginchk(0, 2);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [ax, args] = parseAxis(args);
  if isempty(args)
    varargout{1} = ax.ZTickLabelRotation;
    return
  end
  value = args{1};
  if ~isnumeric(value) || ~isscalar(value) || ~isfinite(value)
    error('Nelson:graphics:mustBeNumericScalar', '%s must be a numeric scalar.', 'Input');
  end
  ax.ZTickLabelRotation = double(value);
  applyZTickAngleTicks(ax);
  if nargout == 1
    error('Nelson:graphics:invalidOutputArgument', 'Invalid output argument.');
  end
end
%=============================================================================
function applyZTickAngleTicks(ax)
  if ~strcmp(ax.ZTickMode, 'auto')
    return
  end
  limits = ax.ZLim;
  if numel(limits) ~= 2 || any(~isfinite(limits)) || limits(1) == limits(2)
    return
  end
  step = zTickAngleStep(abs(diff(limits)) / 10);
  firstTick = ceil(limits(1) / step) * step;
  lastTick = floor(limits(2) / step) * step;
  ticks = firstTick:step:lastTick;
  if numel(ticks) >= 2
    ax.ZTick = ticks;
  end
end
%=============================================================================
function step = zTickAngleStep(rawStep)
  if rawStep <= 0 || ~isfinite(rawStep)
    step = 1;
    return
  end
  scale = 10 ^ floor(log10(rawStep));
  normalized = rawStep / scale;
  if normalized <= 1
    step = scale;
  elseif normalized <= 2
    step = 2 * scale;
  elseif normalized <= 5
    step = 5 * scale;
  else
    step = 10 * scale;
  end
end
%=============================================================================
function [ax, args] = parseAxis(args)
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'axes')
    ax = args{1};
    args = args(2:end);
  else
    ax = gca();
  end
end
%=============================================================================
