%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = streamparticles(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  if usesVertexSyntax(args)
    h = drawStreamParticles(args);
  else
    error('Nelson:graphics:invalidStreamlineVertices', '%s must be a cell array.', 'Streamline vertices');
  end
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function tf = usesVertexSyntax(args)
  tf = false;
  if isempty(args)
    return
  end
  first = args{1};
  if isscalar(first) && isgraphics(first, 'axes')
    args = args(2:end);
    if isempty(args)
      return
    end
    first = args{1};
  end
  tf = iscell(first) || (isscalar(first) && isgraphics(first, 'line'));
end
%=============================================================================
function h = drawStreamParticles(args)
  [parent, targetLine, args] = parseParticleTarget(args);
  [vertices, args] = parseParticleVertices(args);
  [count, args] = parseParticleCount(args);
  options = parseParticleOptions(args);
  if isempty(targetLine) && isempty(parent)
    parent = newplot();
  end
  pts = selectParticlePoints(vertices, count, options.ParticleAlignment);
  if size(pts, 2) == 2
    pts(:, 3) = 0;
  end
  if isempty(targetLine)
    h = line('Parent', parent, 'XData', pts(:, 1).', 'YData', pts(:, 2).', ...
      'ZData', pts(:, 3).');
  else
    h = targetLine;
  end
  set(h, 'LineStyle', 'none', 'Marker', 'o', 'MarkerEdgeColor', 'none', ...
    'MarkerFaceColor', 'red', 'Color', streamParticlesDefaultColor(h));
  if ~isempty(options.LineProperties)
    set(h, options.LineProperties{:});
  end
  h.XData = pts(:, 1).';
  h.YData = pts(:, 2).';
  h.ZData = pts(:, 3).';
  updateParticleAxes(h, pts);
  h.UserData = struct('ChartType', 'streamparticles', 'Vertices', {vertices}, ...
    'Count', count, 'Options', options);
end
%=============================================================================
function color = streamParticlesDefaultColor(h)
  ax = ancestor(h, 'axes');
  if isempty(ax) || isempty(ax.ColorOrder)
    color = [0 0 0];
    return
  end
  color = ax.ColorOrder(1, :);
end
%=============================================================================
function updateParticleAxes(h, pts)
  if isempty(pts)
    return
  end
  ax = ancestor(h, 'axes');
  if isempty(ax)
    return
  end
  if strcmp(ax.XLimMode, 'auto')
    limits = finiteParticleLimits(pts(:, 1));
    if ~isempty(limits)
      ax.XLim = limits;
    end
  end
  if strcmp(ax.YLimMode, 'auto')
    limits = finiteParticleLimits(pts(:, 2));
    if ~isempty(limits)
      ax.YLim = limits;
    end
  end
  if strcmp(ax.ZLimMode, 'auto')
    limits = finiteParticleLimits(pts(:, 3));
    if ~isempty(limits)
      ax.ZLim = limits;
    end
  end
end
%=============================================================================
function limits = finiteParticleLimits(values)
  values = values(isfinite(values));
  if isempty(values)
    limits = [];
    return
  end
  lower = min(values);
  upper = max(values);
  if lower == upper
    lower = lower - 1;
    upper = upper + 1;
  end
  limits = [lower upper];
end
%=============================================================================
function [parent, targetLine, args] = parseParticleTarget(args)
  parent = [];
  targetLine = [];
  if isempty(args)
    return
  end
  first = args{1};
  if isscalar(first) && isgraphics(first, 'axes')
    parent = first;
    args = args(2:end);
  elseif isscalar(first) && isgraphics(first, 'line')
    targetLine = first;
    parent = first.Parent;
    args = args(2:end);
  end
end
%=============================================================================
function [vertices, args] = parseParticleVertices(args)
  if isempty(args) || ~iscell(args{1})
    error('Nelson:graphics:invalidStreamlineVertices', '%s must be a cell array.', 'Streamline vertices');
  end
  vertices = args{1};
  args = args(2:end);
  for k = 1:numel(vertices)
    vertices{k} = validateParticleVertices(vertices{k});
  end
end
%=============================================================================
function pts = validateParticleVertices(pts)
  if ~isnumeric(pts) || ~(size(pts, 2) == 2 || size(pts, 2) == 3)
    error('Nelson:streamparticles:verticesNotNumericArrays', 'Streamline vertices must be numeric arrays with two or three columns.');
  end
  pts = double(pts);
  pts = pts(all(isfinite(pts), 2), :);
end
%=============================================================================
function [count, args] = parseParticleCount(args)
  count = 1;
  if ~isempty(args) && isnumeric(args{1})
    count = double(args{1});
    args = args(2:end);
  end
  if ~isscalar(count) || count <= 0 || ~isfinite(count)
    error('Nelson:graphics:mustBePositiveScalar', '%s must be a positive scalar.', 'Particle count');
  end
end
%=============================================================================
function options = parseParticleOptions(args)
  options = struct('Animate', 0, 'FrameRate', Inf, 'ParticleAlignment', 'off', ...
    'LineProperties', {{}});
  if rem(length(args), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(args)
    name = convertStringsToChars(args{k});
    lowerName = lower(char(name));
    value = args{k + 1};
    switch lowerName
      case 'animate'
        options.Animate = double(value);
      case 'framerate'
        options.FrameRate = double(value);
      case 'particlealignment'
        options.ParticleAlignment = lower(char(value));
      otherwise
        options.LineProperties = [options.LineProperties, {name, value}];
    end
  end
end
%=============================================================================
function pts = selectParticlePoints(vertices, count, alignment)
  if isempty(vertices) || all(cellfun('isempty', vertices))
    pts = zeros(0, 2);
    return
  end
  if strcmp(alignment, 'on')
    pts = selectAlignedParticlePoints(vertices, count);
  else
    pts = cat(1, vertices{:});
    inc = particleIncrement(size(pts, 1), count);
    pts = pts(1:inc:end, :);
  end
end
%=============================================================================
function pts = selectAlignedParticlePoints(vertices, count)
  lengths = cellfun('size', vertices, 1);
  maxLength = max(lengths);
  inc = particleIncrement(maxLength, count);
  pts = zeros(0, size(vertices{find(lengths > 0, 1)}, 2));
  for k = 1:numel(vertices)
    if ~isempty(vertices{k})
      pts = [pts; vertices{k}(1:inc:end, :)];
    end
  end
end
%=============================================================================
function inc = particleIncrement(len, count)
  if count <= 1
    count = count * len;
  end
  inc = ceil(len / count);
  if inc < 1
    inc = 1;
  end
end
%=============================================================================
