%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = feather(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = graphicsParseParent(args);
  [u, v, lineSpec, colorSpec, markerSpec, propertiesList] = parseFeatherArgs(args);
  m = numel(u);
  arrow = ([0 1 NaN .8 1 .8]' + [0 0 NaN .08 0 -.08]' .* 1i);
  z = u + v .* 1i;
  a = arrow * z + ones(6, 1) * (1:m);
  if isempty(parent)
    ax = newplot();
  else
    ax = newplot(parent);
  end
  h = plot(ax, real(a), imag(a), lineSpec, [1 m], [0 0], lineSpec, propertiesList{:});
  if isempty(colorSpec) && ~hasPropertyName(propertiesList, 'Color')
    colorOrder = get(ax, 'ColorOrder');
    if ~isempty(colorOrder)
      singleColor = colorOrder(1, :);
      for k = 1:numel(h)
        h(k).Color = singleColor;
      end
    end
  end
  if isempty(markerSpec) && ~hasPropertyName(propertiesList, 'Marker')
    for k = 1:numel(h)
      h(k).Marker = 'none';
    end
  end
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [u, v, lineSpec, colorSpec, markerSpec, propertiesList] = parseFeatherArgs(args)
  if isempty(args)
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Value');
  end
  if numel(args) >= 2 && isnumeric(args{2})
    u = args{1};
    v = args{2};
    args = args(3:end);
  else
    if ~isnumeric(args{1})
      error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Value');
    end
    z = args{1};
    u = real(z);
    v = imag(z);
    args = args(2:end);
  end
  if numel(u) ~= numel(v)
    error('Nelson:dimagree:valueValueNumberElements', '%s and %s must have the same number of elements.', 'U', 'V');
  end
  if ~(isnumeric(u) && isnumeric(v))
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Value');
  end
  u = double(u(:).');
  v = double(v(:).');
  [lineSpec, colorSpec, markerSpec, propertiesList] = parseLineSpecAndProperties(args);
end
%=============================================================================
function [lineSpec, colorSpec, markerSpec, propertiesList] = parseLineSpecAndProperties(args)
  lineSpec = '-';
  colorSpec = '';
  markerSpec = '';
  if ~isempty(args) && (ischar(args{1}) || isStringScalar(args{1}))
    [~, colorSpec, markerSpec, msg] = colstyle(args{1}, 'plot', false);
    if isempty(msg)
      lineSpec = args{1};
      args = args(2:end);
    else
      colorSpec = '';
      markerSpec = '';
    end
  end
  if mod(numel(args), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  propertiesList = args;
end
%=============================================================================
function tf = hasPropertyName(propertiesList, name)
  tf = false;
  for k = 1:2:numel(propertiesList)
    propertyName = propertiesList{k};
    if (ischar(propertyName) || isStringScalar(propertyName)) && strcmpi(propertyName, name)
      tf = true;
      return
    end
  end
end
%=============================================================================
