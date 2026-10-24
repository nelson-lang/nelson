%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function shading(varargin)
  nargoutchk(0, 0);
  inputArguments = convertStringToCharArgs(varargin);
  if isempty(inputArguments)
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  if isscalar(inputArguments{1}) && isgraphics(inputArguments{1}, 'axes')
    ax = inputArguments{1};
    inputArguments = inputArguments(2:end);
  else
    ax = gca();
  end
  if length(inputArguments) ~= 1 || ~ischar(inputArguments{1})
    error('Nelson:shading:shadingModeExpected', 'A shading mode expected.');
  end
  mode = lower(inputArguments{1});
  h = surfacePatchChildren(ax);
  if isempty(h)
    return
  end
  if strcmp(mode, 'faceted')
    set(h, 'FaceColor', 'flat', 'EdgeColor', [0 0 0]);
  elseif strcmp(mode, 'flat')
    set(h, 'FaceColor', 'flat', 'EdgeColor', 'none');
  elseif strcmp(mode, 'interp')
    set(h, 'FaceColor', 'interp', 'EdgeColor', 'none');
  else
    error('Nelson:shading:invalidPropertyValue', 'Illegal selection for property.');
  end
end
%=============================================================================
