%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = contour(varargin)
  % contour(ax, ...)
  % contour(Z)
  % contour(X, Y, Z)
  % contour(..., levels)
  % contour(..., LineSpec)
  % M = contour(...)
  % [M, c] = contour(...)
  inputArguments = varargin;
  if nargin == 0
    error(_('Wrong number of input arguments.'));
  end
  ax = [];
  if ((length(inputArguments{1})==1) && isgraphics(inputArguments{1},'axes'))
    ax = inputArguments{1};
    inputArguments = inputArguments(2:end);
  else
    ax = newplot();
  end
  axes(ax);
  
  if (length(inputArguments) == 0)
    error(_('Wrong number of input arguments.'));
  end
  % contour(..., LineSpec)
  [ps, cs, ms, msg] = colstyle(inputArguments{end}, 'plot', false);
  propertiesList = struct();    
  r = isempty(msg);
  if (r)
    inputArguments(end) = [];
    propertiesList.LineStyle = ps;
    if isempty(cs)
      propertiesList.EdgeColor = 'flat';
    else
      propertiesList.EdgeColor = cs;
    end
  end
  
  if (length(inputArguments) == 1)
    propertiesList.ZData = inputArguments{1};
    inputArguments = {};
  end
  if (length(inputArguments) == 2)
    if (numel(inputArguments{2}) == 1)
      propertiesList.ZData = inputArguments{1};
      zMin = min(propertiesList.ZData, [], 'all');
      zMax = max(propertiesList.ZData, [], 'all');
      propertiesList.LevelList = linspace(zMin, zMax, inputArguments{2});
    else
      propertiesList.ZData = inputArguments{1};
      propertiesList.LevelList = inputArguments{2};
    end
    inputArguments = {};
  end
  if (length(inputArguments) == 3)
    propertiesList.XData = inputArguments{1};
    propertiesList.YData = inputArguments{2};
    propertiesList.ZData = inputArguments{3};
    inputArguments = {};
  end
  if (length(inputArguments) >= 4)
    propertiesList.XData = inputArguments{1};
    propertiesList.YData = inputArguments{2};
    propertiesList.ZData = inputArguments{3};
    if (numel(inputArguments{4}) == 1)
      zMin = min(propertiesList.ZData(:));
      zMax = max(propertiesList.ZData(:));
      propertiesList.LevelList = linspace(zMin, zMax, inputArguments{4});
    else
      propertiesList.LevelList = inputArguments{4};
    end
    if length(inputArguments) > 4
      inputArguments = inputArguments(5:end);
    else
      inputArguments = {};
    end
  end
  contourProperties = reshape([fieldnames(propertiesList)'; struct2cell(propertiesList)'], 1, []);
  contourProperties = [contourProperties, inputArguments];
  h = __contour__(contourProperties{:});
  axes(ax);
  if (strcmp(ax.XLimMode, 'auto') && strcmp(ax.YLimMode, 'auto'))
    limits = [min(h.XData(:)), max(h.XData(:)), min(h.YData(:)), max(h.YData(:))];
    if ~isempty(limits)
      axis(limits);
    end
  end
  if nargout > 0
    varargout{1} = h.ContourMatrix;
  else
    varargout = {};
  end
  if nargout > 1
    varargout{2} = h;
  end
end
%=============================================================================
