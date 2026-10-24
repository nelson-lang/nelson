%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = stem(varargin)
  % h = stem(Y)
  % h = stem(X, Y)
  % h = stem(ax, Y)
  % h = stem(X, Y, LineSpec)
  % h = stem(ax, X, Y, LineSpec)
  nargoutchk(0, 1);
  narginchk(1, 20);

  [parent, X, Y, automaticX, filled, propertiesLineA, propertiesLineB, callerProperties] = parseArguments(varargin);
  n = size(Y, 2);
  G = [];

  if isgraphics(parent, 'hggroup')
    ax = ancestor(parent, 'axes');
  else
    ax = parent;
  end
  colorOrder = ax.ColorOrder;
  colorIndex = ax.ColorOrderIndex;
  for k = 1:n
    nbColors = size(colorOrder, 1);
    colorIndex = mod(colorIndex, nbColors);
    if (colorIndex == 0)
      colorIndex = nbColors;
    elseif (colorIndex < 0)
      colorIndex = 1;
    end
    color = colorOrder(colorIndex, :);
    if (colorIndex >= nbColors)
      colorIndex = colorIndex + 1;
      colorIndex = mod(colorIndex, nbColors);
      if (colorIndex == 0)
        colorIndex = 1;
      end
    else
      colorIndex = colorIndex + 1;
    end
    g = stemInternal(ax, X(:, k), Y(:, k), color, automaticX, filled, ...
      propertiesLineA, propertiesLineB, callerProperties);
    G = [G, g];
  end
  applyStemXMargin(ax, X);
  if (nargout == 1)
    varargout{1} = G;
  end
end
%=============================================================================
function group = stemInternal(ax, X, Y, color, automaticX, filled, propertiesLineA, propertiesLineB, callerProperties)
  axis(ax, 'auto');

  if isempty(propertiesLineA.Color)
    propertiesLineA.Color = color;
  end
  if isempty(propertiesLineA.MarkerEdgeColor)
    propertiesLineA.MarkerEdgeColor = color;
  end

  if ~isfield(propertiesLineA, 'MarkerFaceColor')
    if filled
      propertiesLineA.MarkerFaceColor = propertiesLineA.Color;
    else
      propertiesLineA.MarkerFaceColor = 'none';
    end
  end

  if isempty(propertiesLineB.Color)
    propertiesLineB.Color = color;
  end

  markerEdgeColor = stemStoredMarkerEdgeColor(propertiesLineA);
  markerFaceColor = stemStoredMarkerFaceColor(propertiesLineA, filled);
  xDataMode = 'manual';
  if automaticX
    xDataMode = 'auto';
  end
  group = __stem__('Parent', ax, 'XData', X(:).', 'YData', Y(:).', ...
    'XDataMode', xDataMode, 'YDataMode', 'manual', ...
    'Color', propertiesLineB.Color, 'LineStyle', propertiesLineB.LineStyle, ...
    'LineWidth', propertiesLineB.LineWidth, 'Marker', propertiesLineA.Marker, ...
    'MarkerSize', propertiesLineA.MarkerSize, 'MarkerEdgeColor', markerEdgeColor, ...
    'MarkerFaceColor', markerFaceColor);

  % The pairs given by the caller belong to the stem itself, not to the lines
  % drawn for it. They are applied before the rendering below reads back the
  % resolved properties, so a BaseValue given by the caller moves the stems.
  if (~isempty(callerProperties))
    set(group, callerProperties{:});
  end

  renderLineA = propertiesLineA;
  renderLineB = propertiesLineB;
  renderLineA.MarkerEdgeColor = stemResolvedMarkerColor(group.MarkerEdgeColor, group.Color);
  renderLineA.MarkerFaceColor = stemResolvedMarkerColor(group.MarkerFaceColor, group.Color);
  renderLineA = reshape([fieldnames(renderLineA)'; struct2cell(renderLineA)'], 1, []);
  renderLineB = reshape([fieldnames(renderLineB)'; struct2cell(renderLineB)'], 1, []);
  renderer = hggroup('Parent', ax);
  renderer.HandleVisibility = 'off';
  H = plot(renderer, X, Y, renderLineA{:});
  baseValue = group.BaseValue;
  n = length(X);
  Xrow = X(:).';
  stemX = reshape([Xrow; Xrow; NaN(1, n)], [], 1);
  stemY = reshape([baseValue * ones(1, n); Y(:).'; NaN(1, n)], [], 1);
  plot(renderer, stemX, stemY, renderLineB{:});
  group.RendererState = struct('ChartType', 'stem', 'Renderer', renderer, 'MarkerLine', H);
end
%=============================================================================
function applyStemXMargin(ax, X)
  % Discrete charts (stem) leave a small symmetric margin along X so the
  % outermost stems do not touch the axes frame. The margin is a fraction of
  % the data span that grows for sparse data and is floored for dense data:
  %   margin = span * max(0.02, 0.8 / pointsPerSeries)
  % Y keeps the standard automatic (tick-aligned) limits, which already
  % include the stem baseline.
  values = X(:);
  values = values(isfinite(values));
  if isempty(values)
    return;
  end
  xmin = min(values);
  xmax = max(values);
  span = xmax - xmin;
  if span > 0
    n = size(X, 1);
    if n < 1
      n = 1;
    end
    margin = span * max(0.02, 0.8 / n);
  else
    margin = 1;
  end
  ax.XLim = [xmin - margin, xmax + margin];
end
%====================================================d=========================
function value = stemStoredMarkerEdgeColor(propertiesLineA)
  value = 'auto';
  if isfield(propertiesLineA, 'MarkerEdgeColor') && ~isempty(propertiesLineA.MarkerEdgeColor) ...
      && ~isequal(propertiesLineA.MarkerEdgeColor, propertiesLineA.Color)
    value = propertiesLineA.MarkerEdgeColor;
  end
end
%=============================================================================
function value = stemStoredMarkerFaceColor(propertiesLineA, filled)
  value = 'none';
  if isfield(propertiesLineA, 'MarkerFaceColor')
    value = propertiesLineA.MarkerFaceColor;
    if filled && isequal(value, propertiesLineA.Color)
      value = 'auto';
    end
  elseif filled
    value = 'auto';
  end
end
%=============================================================================
function value = stemResolvedMarkerColor(value, color)
  if ischar(value) && strcmp(value, 'auto')
    value = color;
  end
end
%====================================================d=========================
function [parent, X, Y, automaticX, filled, propertiesLineA, propertiesLineB, ...
  callerProperties] = parseArguments(inputArguments)
  nbInputArguments = length(inputArguments);
  if (nbInputArguments >= 1) && isscalar(inputArguments{1}) ...
    && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup'))
    parent = newplotTarget(inputArguments{1}(1));
    inputArguments = inputArguments(2:end);
    nbInputArguments = nbInputArguments - 1;
  else
    [parent, inputArguments] = graphicsExtractParentProperty(inputArguments);
    nbInputArguments = length(inputArguments);
    parent = newplotTarget(parent);
  end
  isString = @(x) ischar(x) || isStringScalar(x);
  firstString = find(cellfun(isString, inputArguments), 1);
  if (isempty(firstString))
    firstString = nbInputArguments + 1;
  end
  if ((firstString == 2) || (firstString == 3))
    if (firstString == 2)
      Y = inputArguments{1};
      automaticX = true;
      if isvector(Y)
        X = 1:numel(Y);
      else
        X = repmat((1:size(Y, 1))', 1, size(Y, 2));
      end
      inputArguments = inputArguments(2:end);
    else
      X = inputArguments{1};
      Y = inputArguments{2};
      automaticX = false;
      inputArguments = inputArguments(3:end);
    end
  else
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end

  [linespec, colorspec, markerspec, msg] = colstyle('-o', 'plot');

  filled = false;
  isFilled = @(x)(ischar(x) || isStringScalar(x)) && strcmp(convertStringsToChars(x), 'filled');
  filledDetected = find(cellfun(isFilled, inputArguments));
  if (~isempty(filledDetected))
    filled = true;
    inputArguments(filledDetected) = [];
    nbInputArguments = length(inputArguments);
  end
  nbInputArguments = length(inputArguments);
  if (nbInputArguments > 0)
    % Tested against the stem itself, not against a line: BaseValue and
    % ShowBaseLine are stem properties and were taken for a line spec.
    if ~isValidGraphicsProperty('stem', inputArguments{1})
      param = convertStringsToChars(inputArguments{1});
      if strcmp(param, 'filled')
        filled = true;
      else
        [linespec, colorspec, markerspec, msg] = colstyle(param, 'plot');
        if ~isempty(msg)
          error('Nelson:stem:invalidLineSpec', msg);
        end
      end
      inputArguments = inputArguments(2:end);
      nbInputArguments = length(inputArguments);
    end
  end

  if min(size(X)) == 1, X = X(:); end
  if min(size(Y)) == 1, Y = Y(:); end
  if size(X, 2) == 1 && size(Y, 2) > 1 && size(X, 1) == size(Y, 1)
    X = repmat(X, 1, size(Y, 2));
  end

  propertiesLineA = struct();
  propertiesLineA.Color = colorspec;
  propertiesLineA.MarkerEdgeColor = colorspec;
  propertiesLineA.Marker = markerspec;
  propertiesLineA.MarkerSize = 6;

  % Every pair goes to the stem; only the ones a line understands also shape
  % the lines drawn for it, which would otherwise refuse a stem property.
  callerProperties = {};
  for k = 1:2:nbInputArguments
    callerProperties = [callerProperties, {inputArguments{k}, inputArguments{k + 1}}];
    if isValidGraphicsProperty('line', inputArguments{k})
      propertiesLineA.(inputArguments{k}) = inputArguments{k + 1};
    end
  end
  propertiesLineA.LineStyle = 'none';


  propertiesLineB = struct();
  propertiesLineB.Color = colorspec;
  propertiesLineB.LineStyle = linespec;
  propertiesLineB.LineWidth = 0.5;
  for k = 1:2:nbInputArguments
    if isValidGraphicsProperty('line', inputArguments{k})
      propertiesLineB.(inputArguments{k}) = inputArguments{k + 1};
    end
  end
end
%====================================================d=========================
