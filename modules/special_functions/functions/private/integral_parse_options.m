%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function opts = integral_parse_options(fname, varargin)
  opts = struct('RelTol', 1e-6, 'AbsTol', 1e-10, 'ArrayValued', false, 'Vectorized', true, ...
    'Waypoints', [], 'Method', 'auto');
  oneDimensional = strcmp(fname, 'integral') || strcmp(fname, 'integralInterpolant');
  n = numel(varargin);
  if mod(n, 2) ~= 0
    error(['Nelson:', fname, ':optionsPairs'], _('Options must be given as name/value pairs.'));
  end
  for k = 1:2:n
    name = varargin{k};
    value = varargin{k + 1};
    if ~(ischar(name) || (isstring(name) && isscalar(name)))
      error(['Nelson:', fname, ':optionName'], _('Option name must be a character vector.'));
    end
    name = char(name);
    if strcmpi(name, 'RelativeTolerance') || strcmpi(name, 'RelTol')
      opts.RelTol = integralNonnegativeScalar(fname, 'RelativeTolerance', value);
    elseif strcmpi(name, 'AbsoluteTolerance') || strcmpi(name, 'AbsTol')
      opts.AbsTol = integralNonnegativeScalar(fname, 'AbsoluteTolerance', value);
    elseif strcmpi(name, 'ArrayValued') && oneDimensional
      opts.ArrayValued = integralLogicalScalar(fname, 'ArrayValued', value);
    elseif strcmpi(name, 'Vectorized')
      opts.Vectorized = integralLogicalScalar(fname, 'Vectorized', value);
    elseif strcmpi(name, 'Waypoints') && oneDimensional
      opts.Waypoints = integralWaypoints(fname, value);
    elseif strcmpi(name, 'Waypoints')
      opts.Waypoints = integralGridWaypoints(fname, value, 2 + strcmp(fname, 'integral3'));
    elseif strcmpi(name, 'Method') && strcmp(fname, 'integral3')
      opts.Method = integralMethod(fname, value);
    else
      error(['Nelson:', fname, ':unknownOption'], sprintf(_('Unknown option name: %s'), name));
    end
  end
end
%=============================================================================
function value = integralNonnegativeScalar(fname, name, value)
  if ~(isnumeric(value) && isscalar(value) && isreal(value) && value >= 0)
    error(['Nelson:', fname, ':', name], sprintf('%s must be a nonnegative real scalar.', name));
  end
  value = double(value);
end
%=============================================================================
function value = integralLogicalScalar(fname, name, value)
  if ~(isscalar(value) && (islogical(value) || (isnumeric(value) && isreal(value))))
    error(['Nelson:', fname, ':', name], sprintf(_('%s must be a logical scalar.'), name));
  end
  value = logical(value);
end
%=============================================================================
function value = integralWaypoints(fname, value)
  if ~(isnumeric(value) || islogical(value)) || ~(isvector(value) || isempty(value)) || ~all(isfinite(value(:)))
    error(['Nelson:', fname, ':invalidWaypoints'], _('''Waypoints'' value must be a vector with all finite values.'));
  end
  value = double(value(:).');
end
%=============================================================================
function waypoints = integralGridWaypoints(fname, value, ndims)
  % [x y (z)] array of points or {x y (z)} grid vectors -> one row vector per dimension
  if isempty(value)
    waypoints = {};
    return
  end
  if iscell(value) && numel(value) == ndims
    waypoints = value(:).';
  elseif isnumeric(value) && ismatrix(value) && size(value, 2) == ndims
    waypoints = num2cell(value, 1);
  else
    waypoints = {};
  end
  valid = ~isempty(waypoints);
  for k = 1:numel(waypoints)
    w = waypoints{k};
    valid = valid && (isnumeric(w) || islogical(w)) && (isvector(w) || isempty(w)) && isreal(w) && all(isfinite(w(:)));
    if valid
      waypoints{k} = double(w(:).');
    end
  end
  if ~valid
    error(['Nelson:', fname, ':invalidWaypoints'], sprintf(_('''Waypoints'' value must be a %d-column array or a cell array of %d vectors with real finite values.'), ndims, ndims));
  end
end
%=============================================================================
function method = integralMethod(fname, value)
  method = '';
  if ischar(value) || (isstring(value) && isscalar(value))
    method = lower(char(value));
  end
  if ~any(strcmp(method, {'auto', 'tiled', 'iterated'}))
    error(['Nelson:', fname, ':Method'], _('Method must be ''auto'', ''tiled'', or ''iterated''.'));
  end
end
%=============================================================================
