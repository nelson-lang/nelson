%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = quadgk(fun, a, b, varargin)
  narginchk(3, inf);
  nargoutchk(0, 2);
  if ~isa(fun, 'function_handle')
    error('Nelson:special_functions:funArgNotHandle', 'First argument must be a function handle.');
  end
  if ~(isnumeric(a) && isscalar(a)) || ~(isnumeric(b) && isscalar(b))
    error('Nelson:special_functions:limitsNumericScalars', 'Integration limits must be numeric scalars.');
  end
  opts = quadgkParseOptions(varargin{:});
  points = [a, opts.Waypoints(:).', b];
  if any(~isreal(points)) && any(~isfinite(points))
    error('Nelson:quadgk:complexInfinite', 'Complex contours require finite limits and waypoints.');
  end
  [q, errbnd] = integral_contour(fun, points, opts.RelTol, opts.AbsTol, false, opts.MaxIntervalCount);
  varargout{1} = q;
  if nargout > 1
    varargout{2} = errbnd;
  end
end
%=============================================================================
function opts = quadgkParseOptions(varargin)
  opts = struct('RelTol', 1e-6, 'AbsTol', 1e-10, 'Waypoints', [], 'MaxIntervalCount', 650);
  if mod(numel(varargin), 2) ~= 0
    error('Nelson:special_functions:optionsPairs', 'Options must be given as name/value pairs.');
  end
  for k = 1:2:numel(varargin)
    name = varargin{k};
    value = varargin{k + 1};
    if ~(ischar(name) || (isstring(name) && isscalar(name)))
      error('Nelson:special_functions:optionName', 'Option name must be a character vector.');
    end
    switch lower(char(name))
      case 'reltol'
        opts.RelTol = quadgkNonnegativeScalar(value, 'RelTol');
      case 'abstol'
        opts.AbsTol = quadgkNonnegativeScalar(value, 'AbsTol');
      case 'waypoints'
        if ~isnumeric(value) || ~isvector(value)
          error('Nelson:quadgk:Waypoints', 'Waypoints must be a numeric vector.');
        end
        opts.Waypoints = value(:).';
      case 'maxintervalcount'
        if ~(isnumeric(value) && isscalar(value) && isreal(value) && value >= 1)
          error('Nelson:quadgk:MaxIntervalCount', 'MaxIntervalCount must be a positive real scalar.');
        end
        opts.MaxIntervalCount = double(value);
      otherwise
        error('Nelson:special_functions:unknownOption', 'Unknown option name: %s', char(name));
    end
  end
end
%=============================================================================
function value = quadgkNonnegativeScalar(value, name)
  if ~(isnumeric(value) && isscalar(value) && isreal(value) && value >= 0)
    error(['Nelson:quadgk:', name], sprintf('%s must be a nonnegative real scalar.', name));
  end
  value = double(value);
end
%=============================================================================
