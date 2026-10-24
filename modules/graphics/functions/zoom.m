%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = zoom(varargin)
  % zoom option
  % zoom
  % zoom(factor)
  % zoom(fig, ...)
  % zoom(ax, ...)
  % z = zoom(fig)
  % z = zoom(ax)

  narginchk(0, 2);
  nargoutchk(0, 1);
  optionPosition = 1;
  explicitOption = true;
  targetFig = [];
  if (nargin == 0)
    ax = gca();
    optionOrFactor = 'toggle';
    explicitOption = false;
  elseif (isgraphics(varargin{1}, 'axes'))
    ax = varargin{1};
    [optionOrFactor, optionPosition, explicitOption] = targetOptionLocal(varargin);
  elseif (isgraphics(varargin{1}, 'figure'))
    targetFig = varargin{1};
    ax = targetFig.CurrentAxes;
    [optionOrFactor, optionPosition, explicitOption] = targetOptionLocal(varargin);
  else
    ax = gca();
    if (nargin == 1)
      optionOrFactor = varargin{1};
    else
      optionOrFactor = 'toggle';
    end
    optionPosition = 1;
  end
  if isempty(ax)
    ax = gca();
  end
  if isempty(targetFig)
    targetFig = ancestor(ax, 'figure');
  end
  % A bare figure or axes handle requested with an output simply returns the
  % zoom mode object; it must not toggle the current zoom state.
  if (nargout == 0 || explicitOption)
    if isnumeric(optionOrFactor) && isscalar(optionOrFactor)
      factor = optionOrFactor;
      __zoom__(ax, factor);
    elseif ((ischar(optionOrFactor) && isrow(optionOrFactor)) || isStringScalar(optionOrFactor))
      option = convertStringsToChars(optionOrFactor);
      mustBeMember(tolower(option), {'on', 'off', 'reset', 'out', 'xon', 'yon', 'toggle'}, optionPosition);
      __zoom__(ax, option);
    else
      error('Nelson:graphics:wrongTypeForInput', 'Wrong type for input arguments.');
    end
  end
  if (nargout > 0)
    varargout{1} = ZoomObject(targetFig);
  end
end
%=============================================================================
function [optionOrFactor, optionPosition, explicitOption] = targetOptionLocal(args)
  % A figure or axes handle may be given on its own, in which case zoom is
  % simply toggled on that target; an option or factor is the second argument
  % when present.
  if numel(args) >= 2
    optionOrFactor = args{2};
    optionPosition = 2;
    explicitOption = true;
  else
    optionOrFactor = 'toggle';
    optionPosition = 1;
    explicitOption = false;
  end
end
%=============================================================================
