%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = yticks(varargin)
  narginchk(0, 2);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [ax, args] = parseAxis(args);
  if isempty(args)
    varargout{1} = ax.YTick;
    return
  end
  value = args{1};
  if ischar(value)
    switch lower(value)
      case 'mode'
        varargout{1} = ax.YTickMode;
        return
      case 'auto'
        ax.YTickMode = 'auto';
      case 'manual'
        ax.YTickMode = 'manual';
      otherwise
        error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
    end
  elseif isnumeric(value)
    if ~isvector(value) && ~isempty(value)
      error('Nelson:graphics:invalidInput', 'Input must be a vector of numeric values.');
    end
    ax.YTick = value(:)';
    ax.YTickMode = 'manual';
  else
    error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
  end
  if nargout == 1
    error('Nelson:graphics:invalidOutputArgument', 'Invalid output argument.');
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
