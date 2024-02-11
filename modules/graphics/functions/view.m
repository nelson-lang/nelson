%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = view(varargin)
  % view(az, el)
  % view([az, el])
  % view([x, y, z])
  % view(2)
  % view(3)
  % view(ax, ...)
  % [az, el] = view(...)
  parsedArguments = parseArguments(varargin);
  ax = parsedArguments{1};
  
  if length(parsedArguments) == 3
    az = parsedArguments{2};
    el = parsedArguments{3};
    ax.View = [az, el];
  end
  vw = ax.View;
  switch nargout()
    case 0
      varargout = {};
    case 1
      varargout{1} = vw;
    case 2
      varargout{1} = vw(1);
      varargout{2} = vw(2);
    otherwise
  end
end
%=============================================================================
function parsedArguments = parseArguments(inputArguments)
  parsedArguments = {};
  if (isgraphics(inputArguments{1}, 'axes'))
    ax = inputArguments{1};
    inputArguments = inputArguments(2:end);
  else
    ax = gca();
  end
  parsedArguments{1} = ax;
  if length(inputArguments) > 0
    [az, el] = parseAzimuthAndElevation(inputArguments);
    parsedArguments{2} = az;
    parsedArguments{3} = el;
  end
end
%=============================================================================
function [az, el] = parseAzimuthAndElevation(inputArguments)
  if isempty(inputArguments)
    az = 0;
    el = 90;
  elseif numel(inputArguments) == 1
    [az, el] = parseSingleArgument(inputArguments{1});
  elseif numel(inputArguments) == 2
    az = inputArguments{1};
    el = inputArguments{2};
  else
    error(_('Invalid input arguments.'));
  end
end
%=============================================================================
function [az, el] = parseSingleArgument(arg)
  if isnumeric(arg)
    if isscalar(arg)
      [az, el] = handleScalarArgument(arg);
    elseif isvector(arg) && length(arg) == 2
      az = arg(1);
      el = arg(2);
    elseif isvector(arg) && length(arg) == 3
      [az, el] = handleVectorArgument(arg);
    else
      error(_('Invalid input arguments.'));
    end
  else
    error(_('Invalid input arguments.'));
  end
end
%=============================================================================
function [az, el] = handleScalarArgument(d)
  if d == 2
    az = 0;
    el = 90;
  elseif d == 3
    az = -37.5;
    el = 30;
  else
    error(_('Invalid input arguments.'));
  end
end
%=============================================================================
function [az, el] = handleVectorArgument(xyz)
  [az, el] = cart2sph(xyz(1), xyz(2), xyz(3));
  az = az * (180 / pi);
  if az ~= 0
    az = az + 90;
  end
  el = el * (180 / pi);
end
%=============================================================================
