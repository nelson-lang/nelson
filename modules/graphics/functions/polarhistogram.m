%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = polarhistogram(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [ax, args] = polarParseTargetAxes(args);
  if isempty(args) || ~isnumeric(args{1})
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Angle data');
  end
  theta = double(args{1}(:));
  args = args(2:end);
  opts = parsePolarHistogramOptions(args);
  if isempty(opts.BinEdges) && ~isempty(opts.NumBins)
    opts.BinEdges = polarNumBinsEdges(theta, opts.NumBins);
  end
  [fig, previousDrawLater] = polarBeginDrawLater(ax);
  try
    ax = polarPrepareDataAxes(ax);
    h = createPolarHistogramObject(ax, theta, opts);
    counts = h.Values;
    edges = h.BinEdges;
    patches = drawPolarHistogramPatches(ax, counts, edges, opts);
    limitHandle = __line__('Parent', ax, 'XData', [], 'YData', [], ...
      'ThetaData', edges(1:end - 1)', 'RData', counts(:), ...
      'Visible', 'off', 'HandleVisibility', 'off');
    polarAppendDataHandles(ax, limitHandle);
    state = polarHistogramState(theta, counts, edges, patches, limitHandle, opts);
    h.UserData = state;
    if ~isempty(patches)
      patches(1).UserData = state;
    end
    polarEndDrawLater(fig, previousDrawLater);
  catch exception
    polarEndDrawLater(fig, previousDrawLater);
    error(exception.message);
  end
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function opts = parsePolarHistogramOptions(args)
  defaultEdgeColor = graphicsDefaultEdgeColor();
  opts = struct('NumBins', [], 'BinEdges', [], 'Normalization', 'count', ...
    'FaceColor', 'auto', 'EdgeColor', defaultEdgeColor, 'FaceAlpha', 0.6);
  if ~isempty(args) && isnumeric(args{1})
    if isscalar(args{1})
      opts.NumBins = validateNumBins(args{1});
    else
      opts.BinEdges = validateEdges(args{1});
    end
    args = args(2:end);
  end
  if rem(length(args), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(args)
    name = lower(char(args{k}));
    value = args{k + 1};
    switch name
      case 'numbins'
        opts.NumBins = validateNumBins(value);
      case 'binedges'
        opts.BinEdges = validateEdges(value);
      case 'normalization'
        opts.Normalization = validateNormalization(value);
      case 'facecolor'
        opts.FaceColor = value;
      case 'edgecolor'
        opts.EdgeColor = value;
      case 'facealpha'
        opts.FaceAlpha = validateAlpha(value);
      otherwise
        error('Nelson:graphics:unsupportedProperty', 'Unsupported "%s" option.', 'polarhistogram');
    end
  end
end
%=============================================================================
function edges = polarNumBinsEdges(theta, numBins)
  theta = theta(isfinite(theta));
  if isempty(theta)
    edges = linspace(0, 2 * pi, numBins + 1);
    return
  end
  quantum = pi / 4;
  firstEdge = floor(min(theta) / quantum) * quantum;
  lastEdge = ceil(max(theta) / quantum) * quantum;
  if firstEdge == lastEdge
    lastEdge = firstEdge + quantum;
  end
  edges = linspace(firstEdge, lastEdge, numBins + 1);
end
%=============================================================================
function patches = drawPolarHistogramPatches(parent, counts, edges, opts)
  patches = gobjects(1, numel(counts));
  faceColor = polarHistogramPatchFaceColor(parent, opts.FaceColor);
  for k = 1:numel(counts)
    theta = linspace(edges(k), edges(k + 1), 12);
    radius = counts(k) * ones(size(theta));
    x = [0, radius .* cos(theta), 0];
    y = [0, radius .* sin(theta), 0];
    patches(k) = __patch__('Parent', parent, 'XData', x, 'YData', y, ...
      'FaceColor', faceColor, 'EdgeColor', opts.EdgeColor, ...
      'FaceAlpha', opts.FaceAlpha, 'FaceLighting', 'none', ...
      'EdgeLighting', 'none');
  end
end
%=============================================================================
function color = polarHistogramPatchFaceColor(parent, value)
  if ischar(value) && strcmpi(value, 'auto')
    color = parent.ColorOrder(1, :);
    return
  end
  color = value;
end
%=============================================================================
function h = createPolarHistogramObject(parent, theta, opts)
  args = {'Parent', parent, 'Data', theta, 'Normalization', opts.Normalization, ...
    'FaceColor', opts.FaceColor, 'EdgeColor', opts.EdgeColor, 'FaceAlpha', opts.FaceAlpha};
  if ~isempty(opts.BinEdges)
    args = [args, {'BinEdges', opts.BinEdges}];
  elseif ~isempty(opts.NumBins)
    args = [args, {'NumBins', opts.NumBins}];
  end
  h = __histogram__(args{:});
end
%=============================================================================
function state = polarHistogramState(theta, counts, edges, patches, limitHandle, opts)
  state = struct();
  state.ChartType = 'polarhistogram';
  state.Data = theta;
  state.Values = counts;
  state.BinEdges = edges;
  state.Patches = patches;
  state.LimitHandle = limitHandle;
  state.Options = opts;
end
%=============================================================================
function value = validateNumBins(value)
  if ~isnumeric(value) || ~isscalar(value) || value < 1
    error('Nelson:graphics:mustBePositiveScalar', '%s must be a positive scalar.', 'NumBins');
  end
  value = floor(double(value));
end
%=============================================================================
function edges = validateEdges(value)
  if ~isnumeric(value) || numel(value) < 2 || any(diff(value(:)) <= 0)
    error('Nelson:polarhistogram:invalidBinEdges', 'BinEdges must be a strictly increasing numeric vector.');
  end
  edges = double(value(:)');
end
%=============================================================================
function value = validateNormalization(value)
  value = lower(char(value));
  if ~(strcmp(value, 'count') || strcmp(value, 'probability'))
    error('Nelson:polarhistogram:invalidNormalization', 'Normalization must be ''count'' or ''probability''.');
  end
end
%=============================================================================
function value = validateAlpha(value)
  if ~isnumeric(value) || ~isscalar(value) || value < 0 || value > 1
    error('Nelson:polarhistogram:invalidFaceAlpha', 'FaceAlpha must be between 0 and 1.');
  end
end
%=============================================================================
