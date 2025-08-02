%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = grid(varargin)
  narginchk(0, 2);
  nargoutchk(0, 0);
  
  if (nargin == 0)
    ax = gca();
    gridToggle(ax);
    return;
  end
  inputArguments = varargin;
  if isgraphics(inputArguments{1})
    ax = inputArguments{1};
    inputArguments = inputArguments(2:end);
  else
    ax = gca();
  end
  
  if length(inputArguments) == 0
    gridToggle(ax);
  else
    if ischar(inputArguments)
      parameter = inputArguments;
    else
      parameter = inputArguments{1};
    end
    if ischar(parameter)
      if strcmp(parameter, 'on')
        gridOn(ax);
      elseif strcmp(parameter, 'off')
        gridOff(ax);
      elseif strcmp(parameter, 'minor')
        minorGridToggle(ax);
      else
        error(_('Unknown command option.'));
      end 
    end
  end
end
%=============================================================================
function minorGridOn(go)
  go.XMinorGrid = 'on';
  go.YMinorGrid = 'on';
  go.ZMinorGrid = 'on';
end
%=============================================================================
function minorGridOff(go)
  go.XMinorGrid = 'off';
  go.YMinorGrid = 'off';
  go.ZMinorGrid = 'off';
end
%=============================================================================
function gridOff(go)
  go.XGrid = 'off';
  go.YGrid = 'off';
  go.ZGrid = 'off';
end
%=============================================================================
function gridOn(go)
  go.XGrid = 'on';
  go.YGrid = 'on';
  go.ZGrid = 'on';
end
%=============================================================================
function gridOff(go)
  go.XGrid = 'off';
  go.YGrid = 'off';
  go.ZGrid = 'off';
end
%=============================================================================
function gridToggle(go)
  if (strcmp(go.XGrid, 'on') || strcmp(go.YGrid,'on') || strcmp(go.ZGrid,'on'))
    gridOff(go);
  else
    gridOn(go);
  end
end
%=============================================================================
function minorGridToggle(go)
  if ~isprop(go, 'XMinorGrid')
    return
  end
  if (strcmp(go.XMinorGrid, 'on') || strcmp(go.YMinorGrid, 'on') || strcmp(go.ZMinorGrid, 'on'))
    minorGridOff(go);
  else
    minorGridOn(go);
  end
end
%=============================================================================
