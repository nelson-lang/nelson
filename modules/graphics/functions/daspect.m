%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = daspect(varargin)
  narginchk(0, 2);
  nargoutchk(0, 1);
  
  % Handle zero input: return current axes DataAspectRatio
  if nargin == 0
    currentAxes = gca();
    varargout{1} = currentAxes.DataAspectRatio;
    return
  end
  
  % Parse first argument
  firstArg = varargin{1};
  firstArg = convertStringsToChars(firstArg);
  
  % Parse second argument if present
  if nargin > 1
    secondArg = varargin{2};
    secondArg = convertStringsToChars(secondArg);
  end
  
  % Determine axes and value to set/query
  if length(firstArg) == 1 && isgraphics(firstArg, 'axes')
    axesHandle = firstArg;
    if nargin == 2
      aspectValue = secondArg;
    else
      varargout{1} = axesHandle.DataAspectRatio;
      return
    end
  else
    axesHandle = gca();
    aspectValue = firstArg;
  end
  
  % Handle string value: mode query/set
  if ischar(aspectValue)
    if strcmp(aspectValue, 'mode')
      varargout{1} = axesHandle.DataAspectRatioMode;
    else
      axesHandle.DataAspectRatioMode = aspectValue;
    end
    return
  end
  
  % Otherwise, set DataAspectRatio
  axesHandle.DataAspectRatio = aspectValue;
end
%=============================================================================
