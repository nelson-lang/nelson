%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = plot3(varargin)
  inputArguments = varargin;
  nbInputArguments = nargin;
  
  if (nbInputArguments >= 2)
    if (isscalar(inputArguments{1}) && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup')))
      go = inputArguments{1}(1);
      inputArguments(1) = [];
      nbInputArguments = nbInputArguments - 1;
    else   
      go = newplot();
    end
  end
  backupCurrentAxis = gca();
  if isgraphics(go, 'hggroup')
    ax = ancestor(go, 'axes');
    axes(ax);
  else
    axes(go);
  end
  propertyIndex = 0;
  if (nbInputArguments > 2)
    propertyIndex = nbInputArguments - 1;
    bContinue = true;
    while ((propertyIndex >= 1) && bContinue)
      bContinue = ischar(inputArguments{propertyIndex}) && isValidGraphicsProperty('line', inputArguments{propertyIndex});
      if bContinue
        propertyIndex = propertyIndex - 2;
      end
    end
    propertyIndex = propertyIndex + 2;
  end
  propertiesList = {};
  
  if ((propertyIndex > 0) && (propertyIndex < nbInputArguments))
    propertiesList = inputArguments(propertyIndex:end);
    inputArguments(propertyIndex:end) = [];
  end
  
  h = [];
  while (~isempty(inputArguments))
    cs = '';
    ms = ''; 
    ps = '';
    if (length(inputArguments) < 3)
      error(_('Not enough input arguments.'));
    end;
    r = true;
    if (length(inputArguments) > 3)
      [ps, cs, ms, msg] = colstyle(inputArguments{4}, '', false);
      r = isempty(msg);
    end
    if (length(inputArguments) == 3 || (length(inputArguments) > 3) && ~r)
      h = [h; plot_XYZ(inputArguments{1}, inputArguments{2}, inputArguments{3}, go, propertiesList)];
      inputArguments(1:3) = [];
    elseif ((length(inputArguments) >= 4) && r)
      h = [h; plot_XYZ(inputArguments{1}, inputArguments{2}, inputArguments{3}, go, completeProperties(cs,ms,ps,propertiesList))];
      inputArguments(1:4) = [];
    end;
  end
  if ~ishold()
    view(3);
  end
  axes(backupCurrentAxis);
  if isempty(find(strcmp(propertiesList, 'Visible')))
    h.Visible = 'on';
  end
  if (nargout > 0)
    varargout{1} = h;
  end
end
%=============================================================================
function q = completeProperties(cs, ms, ps, p)
  if isempty(ps)
    ps = 'none';
  end 
  if (strcmp(cs, ''))
    q = {'Marker', ms, 'LineStyle', ps, p{:}};
  else
    q = {'Color', cs, 'Marker', ms, 'LineStyle', ps, 'MarkerEdgeColor', cs, 'MarkerFaceColor', cs, p{:}};
  end
end
%=============================================================================
function h = plot_XYZ(X, Y, Z, go, lineProperties)
  h = [];
  if ((isvector(X) || isvector(Y) || isvector(Z)) && (~isvector(X) || ~isvector(Y) || ~isvector(Z)))
    rows = max([size(X,1),size(Y,1),size(Z,1)]);
    cols = max([size(X,2),size(Y,2),size(Z,2)]);
    X = resizeMatrix(X, rows, cols);
    Y = resizeMatrix(Y, rows, cols);
    Z = resizeMatrix(Z, rows, cols);
  end
  if (isvector(X))
    X = X(:);
  end;
  if (isvector(Y))
    Y = Y(:);
  end;
  if (isvector(Z))
    Z = Z(:);
  end;
  for i=1:size(Z, 2)
    h = [h; plotVector(go, X(:, i), Y(:, i), Z(:, i), lineProperties)];
  end
end
%=============================================================================
function x = resizeMatrix(a, rows, cols)
  if (length(a) == rows)
    x = repmat(a(:), [1, cols]);
  elseif (length(a) == cols)
    x = repmat(a(:)', [rows,1]);
  else
    error(_('Wrong size for input arguments: vectors with same size expected.'));
  end
end
%=============================================================================
function hl = plotVector(go, x, y, z, lineProperties)
  if isgraphics(go, 'hggroup')
    ax = ancestor(go, 'axes');
  else
    ax = go;
  end  
  lineStyle = getLineStyleAndUpdateIndex(ax);
  color = getColorAndUpdateIndex(ax);
  if (~any(strcmp(lineProperties, 'LineStyle')))
    lineProperties = [lineProperties, {'LineStyle', lineStyle}];
  end  
  hl = __line__('Parent', go, 'XData', x, 'YData', y, 'ZData', z, 'Color', color, 'Visible', 'off', lineProperties{:});
end
%=============================================================================
