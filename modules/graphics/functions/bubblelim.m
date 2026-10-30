%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = bubblelim(varargin)
  varargin = convertStringToCharArgs(varargin);
  narginchk(0, 2);
  nargoutchk(0, 1);
  [ax, args] = parseBubbleAxes(varargin);
  if isempty(args)
    varargout{1} = currentBubbleLimits(ax);
    return
  end
  value = args{1};
  if ischar(value)
    switch lower(value)
      case 'mode'
        varargout{1} = ax.BubbleLimitsMode_Hidden;
        return
      case 'auto'
        ax.BubbleLimitsMode_Hidden = 'auto';
        rejectOutput(nargout);
        return
      case 'manual'
        ax.BubbleLimits_Hidden = currentBubbleLimits(ax);
        ax.BubbleLimitsMode_Hidden = 'manual';
        rejectOutput(nargout);
        return
      otherwise
        error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
    end
  end
  validateBubbleLimits(value);
  ax.BubbleLimits_Hidden = double(value(:).');
  ax.BubbleLimitsMode_Hidden = 'manual';
  rejectOutput(nargout);
end
%=============================================================================
function [ax, args] = parseBubbleAxes(args)
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'axes')
    ax = args{1};
    args = args(2:end);
  else
    ax = gca();
  end
end
%=============================================================================
function limits = currentBubbleLimits(ax)
  if strcmp(ax.BubbleLimitsMode_Hidden, 'manual')
    limits = ax.BubbleLimits_Hidden;
  else
    limits = autoBubbleLimits(ax);
    ax.BubbleLimits_Hidden = limits;
  end
end
%=============================================================================
function limits = autoBubbleLimits(ax)
  values = [];
  children = ax.Children;
  for k = 1:numel(children)
    child = children(k);
    if isgraphics(child, 'bubblechart')
      values = [values, double(child.SizeData(:).')];
    end
  end
  values = values(isfinite(values));
  values(values < 0) = 0;
  if isempty(values)
    limits = [0 1];
  else
    limits = [min(values), max(values)];
    if limits(1) == limits(2)
      if limits(2) > 0
        limits(1) = limits(2) * 0.9;
      else
        limits(2) = 1;
      end
    end
  end
end
%=============================================================================
function validateBubbleLimits(value)
  if ~(isnumeric(value) && isreal(value) && numel(value) == 2 && ...
      all(isfinite(value)) && value(2) > value(1))
    error('Nelson:bubblelim:invalidValue', 'Value must be a 1x2 vector of numeric type in which the second element is larger than the first.');
  end
end
%=============================================================================
function rejectOutput(nout)
  if nout > 0
    error('Nelson:tooManyOutputs', 'Too many output arguments.');
  end
end
%=============================================================================
