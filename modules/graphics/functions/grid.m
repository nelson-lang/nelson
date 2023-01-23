%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
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
    propertyIndex = 2;
  else
    ax = gca();
    propertyIndex = 1;
  end
  if (nargin - propertyIndex == 0)
    gridToggle(ax);
  elseif (strcmp(varargin{propertyIndex}, 'on'))
    gridOn(ax);
  elseif (strcmp(varargin{propertyIndex}, 'off'))
    gridOff(gca);
  else
    error(_('Unknown command option.'));
  end
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
