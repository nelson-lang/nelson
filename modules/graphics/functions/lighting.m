%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function lighting(varargin)
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
    error('Nelson:lighting:lightingModeExpected', 'A lighting mode expected.');
  end
  mode = lower(inputArguments{1});
  if ~any(strcmp(mode, {'none', 'flat', 'gouraud', 'phong'}))
    error('Nelson:lighting:invalidPropertyValue', 'Illegal selection for property.');
  end
  h = surfacePatchChildren(ax);
  if ~isempty(h)
    set(h, 'FaceLighting', mode, 'EdgeLighting', mode);
  end
end
%=============================================================================
