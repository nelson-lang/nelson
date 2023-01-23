%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = mesh(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  
  inputArguments = varargin;
  nbInputArguments = nargin;
  if (nbInputArguments >= 2)
    if ((length(inputArguments{1}) == 1) && isgraphics(inputArguments{1},'axes'))
      go = inputArguments{1}(1);
    else   
      go = newplot();
    end
  else
    go = newplot();
  end
  nextplot = go.NextPlot;
  
  propertyArg = find(cellfun ('isclass', varargin, 'char'), 1);
  if isempty(propertyArg)
    h = surface(inputArguments{:});
    h.FaceColor ='w';
    h.EdgeColor = 'flat';
    h.FaceLighting = 'none';
    h.EdgeLighting = 'flat';
  else
    h = surface(inputArguments{:});
    h.FaceColor ='w';
    h.EdgeColor = 'flat';
    h.FaceLighting = 'none';
    h.EdgeLighting = 'flat';
    propertiesList = inputArguments(propertyArg:end);
    for k = 1:2:length(propertiesList)
      name = propertiesList{k};
      value = propertiesList{k + 1};
      set(h, name, value);
    end
  end
  if (~ishold(go))
    view(go, 3);
    go.XGrid = 'on';
    go.YGrid = 'on';
    go.ZGrid = 'on';
  end
  if nargout > 0
    varargout{1} = h;
  end
end
%=============================================================================
