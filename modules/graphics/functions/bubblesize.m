%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = bubblesize(varargin)
  varargin = convertStringToCharArgs(varargin);
  narginchk(0, 2);
  nargoutchk(0, 1);
  [ax, args] = parseBubbleAxes(varargin);
  if isempty(args)
    varargout{1} = ax.BubbleSizeRange_Hidden;
    return
  end
  value = args{1};
  validateBubbleSize(value);
  ax.BubbleSizeRange_Hidden = double(value(:).');
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
function validateBubbleSize(value)
  if ~(isnumeric(value) && isreal(value) && numel(value) == 2 && ...
      all(isfinite(value)) && all(value > 0) && value(2) > value(1))
    error('Nelson:bubblesize:invalidValue', 'Value must be a 1x2 vector of positive numbers in which the second element is larger than the first.');
  end
end
%=============================================================================
function rejectOutput(nout)
  if nout > 0
    error('Nelson:tooManyOutputs', 'Too many output arguments.');
  end
end
%=============================================================================
