%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = pcolor(varargin)
  inputArguments = convertStringToCharArgs(varargin);
  target = [];
  if (length(inputArguments) >= 2)
    if (isscalar(inputArguments{1}) && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup')))
      target = inputArguments{1}(1);
      inputArguments = inputArguments(2:end);
    end
  end
  if isempty(target)
    % The pair is pulled out because the surface is given a parent of its own.
    [target, inputArguments] = graphicsExtractParentProperty(inputArguments);
  end
  if isempty(target)
    target = gca();
  elseif isgraphics(target, 'hggroup')
    % The cells are drawn in an axes, so a group designated as target resolves
    % to the axes holding it, and that axes is the one to start anew.
    target = ancestor(target, 'axes');
  end
  % Split the data from the name/value pairs: without this a property name is
  % taken for one of the X, Y, C arguments.
  firstProperty = find(cellfun('isclass', inputArguments, 'char'), 1);
  if isempty(firstProperty)
    firstProperty = length(inputArguments) + 1;
  end
  propertiesList = inputArguments(firstProperty:end);
  inputArguments = inputArguments(1:firstProperty - 1);
  nextPlot = target.NextPlot;
  % Applying the NextPlot policy is what honours hold on: a plain cla() here
  % cleared the axes even when NextPlot was 'add'.
  ax = newplotTarget(target);

  % The default edge colour comes before the caller properties so that an
  % EdgeColor given to pcolor wins over it.
  defaultProperties = {'EdgeColor', graphicsDefaultEdgeColor()};
  if (length(inputArguments) == 1)
    C = inputArguments{1};
    h = surface(ax, zeros(size(C)), C, defaultProperties{:}, propertiesList{:});
    [m, n] = size(C);
    xlims = [1, n];
    ylims = [1, m];
  elseif (length(inputArguments) == 3)
    X = inputArguments{1};
    Y = inputArguments{2};
    C = inputArguments{3};
    Z = zeros(size(C));
    h = surface(ax, X, Y, Z, C, defaultProperties{:}, propertiesList{:});
    xlims = [min(min(X)), max(max(X))];
    ylims = [min(min(Y)), max(max(Y))];
  else
    error('Nelson:pcolor:pcolorRequiresEitherX', 'pcolor requires either X,Y,C, or C arguments.');
  end
  axis('tight');
  if ismember(nextPlot, {'replace', 'replaceall'})
    if xlims(2) <= xlims(1)
      xlims(2) = xlims(1) + 1;
    end
    if ylims(2) <= ylims(1)
      ylims(2) = ylims(1) + 1;
    end
    xlim(ax, xlims);
    ylim(ax, ylims);
  end
  if (nargout > 0)
    varargout{1} = h;
  end
end
%=============================================================================
