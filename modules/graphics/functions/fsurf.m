%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = fsurf(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  inputArguments = convertStringToCharArgs(varargin);
  if isscalar(inputArguments{1}) && isgraphics(inputArguments{1}, 'axes')
    ax = newplotTarget(inputArguments{1});
    inputArguments = inputArguments(2:end);
  else
    ax = newplotTarget(graphicsParentProperty(inputArguments));
  end
  [fun, xrange, yrange, meshDensity, xrangeSet, yrangeSet, propertiesList] = parseFsurfInputs(inputArguments);
  if ~isnumeric(meshDensity) || ~isscalar(meshDensity) || meshDensity < 2 || meshDensity ~= fix(meshDensity)
    error('Nelson:graphics:invalidMeshDensity', 'MeshDensity must be a positive integer greater than one.');
  end
  x = linspace(xrange(1), xrange(2), meshDensity);
  y = linspace(yrange(1), yrange(2), meshDensity);
  [X, Y] = meshgrid(x, y);
  Z = evaluateFunctionSurface(fun, X, Y);
  rangeProperties = {};
  if xrangeSet
    rangeProperties = [rangeProperties, {'XRange', xrange}];
  end
  if yrangeSet
    rangeProperties = [rangeProperties, {'YRange', yrange}];
  end
  h = __fsurf__('Parent', ax, 'Function', fun, rangeProperties{:}, 'MeshDensity', ...
    meshDensity, 'XData', X, 'YData', Y, 'ZData', Z, propertiesList{:});
  view(ax, 3);
  xlim(ax, xrange);
  ylim(ax, yrange);
  grid(ax, 'on');
  if nargout > 0
    varargout{1} = h;
  end
end
%=============================================================================
function [fun, xrange, yrange, meshDensity, xrangeSet, yrangeSet, propertiesList] = parseFsurfInputs(args)
  if isempty(args)
    error('Nelson:graphics:functionExpected', 'Function expected.');
  end
  fun = args{1};
  args = args(2:end);
  xrange = [-5 5];
  yrange = [-5 5];
  xrangeSet = false;
  yrangeSet = false;
  meshDensity = 35;
  if ~isempty(args) && isnumeric(args{1}) && isvector(args{1})
    range = args{1}(:)';
    if numel(range) == 2
      xrange = range;
      yrange = range;
      xrangeSet = true;
      yrangeSet = true;
    elseif numel(range) == 4
      xrange = range(1:2);
      yrange = range(3:4);
      xrangeSet = true;
      yrangeSet = true;
    else
      error('Nelson:graphics:invalidRange', 'Range must have two or four elements.');
    end
    args = args(2:end);
  end
  k = 1;
  while k <= length(args) - 1
    if ~ischar(args{k})
      break
    end
    name = lower(args{k});
    if strcmp(name, 'meshdensity')
      meshDensity = args{k + 1};
      args(k:k + 1) = [];
    elseif strcmp(name, 'xrange')
      xrange = args{k + 1};
      xrangeSet = true;
      args(k:k + 1) = [];
    elseif strcmp(name, 'yrange')
      yrange = args{k + 1};
      yrangeSet = true;
      args(k:k + 1) = [];
    else
      k = k + 2;
    end
  end
  propertiesList = args;
end
%=============================================================================
function Z = evaluateFunctionSurface(fun, X, Y)
  try
    Z = feval(fun, X, Y);
  catch
    Z = zeros(size(X));
    for k = 1:numel(X)
      Z(k) = feval(fun, X(k), Y(k));
    end
  end
  if ~isequal(size(Z), size(X))
    error('Nelson:fsurf:invalidFunction', 'Function must return an array the same size as X and Y.');
  end
end
%=============================================================================
