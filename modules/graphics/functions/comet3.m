%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function comet3(varargin)
  narginchk(1, 5);
  nargoutchk(0, 0);
  args = convertStringToCharArgs(varargin);
  [ax, args] = parseComet3Parent(args);
  [x, y, z, p] = parseComet3Data(args);
  cometAnimate(ax, x, y, z, p);
end
%=============================================================================
function [ax, args] = parseComet3Parent(args)
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'axes')
    ax = args{1};
    args = args(2:end);
  else
    ax = [];
  end
end
%=============================================================================
function [x, y, z, p] = parseComet3Data(args)
  p = 0.1;
  if numel(args) == 1
    z = args{1};
    x = 1:numel(z);
    y = 1:numel(z);
  elseif numel(args) == 3
    x = args{1};
    y = args{2};
    z = args{3};
  elseif numel(args) == 4
    x = args{1};
    y = args{2};
    z = args{3};
    p = args{4};
  else
    error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
  end
  if ~isnumeric(x) || ~isnumeric(y) || ~isnumeric(z) || ~isvector(x) || ...
      ~ isvector(y) || ~isvector(z) || numel(x) ~= numel(y) || ...
      numel(x) ~= numel(z) || isempty(x)
    error('Nelson:comet3:invalidXYAnd', 'X, Y, and Z must be nonempty numeric vectors with the same number of elements.');
  end
  if ~isnumeric(p) || ~isscalar(p) || ~isfinite(p) || p < 0 || p >= 1
    error('Nelson:comet:invalidP', 'P must be a scalar value in the interval [0, 1).');
  end
  x = x(:).';
  y = y(:).';
  z = z(:).';
end
%=============================================================================
