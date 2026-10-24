%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function comet(varargin)
  narginchk(1, 4);
  nargoutchk(0, 0);
  args = convertStringToCharArgs(varargin);
  [ax, args] = parseCometParent(args);
  [x, y, p] = parseCometData(args);
  cometAnimate(ax, x, y, [], p);
end
%=============================================================================
function [ax, args] = parseCometParent(args)
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'axes')
    ax = args{1};
    args = args(2:end);
  else
    ax = [];
  end
end
%=============================================================================
function [x, y, p] = parseCometData(args)
  p = 0.1;
  if numel(args) == 1
    y = args{1};
    x = 1:numel(y);
  elseif numel(args) == 2
    x = args{1};
    y = args{2};
  elseif numel(args) == 3
    x = args{1};
    y = args{2};
    p = args{3};
  else
    error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
  end
  if ~isnumeric(x) || ~isnumeric(y) || ~isvector(x) || ~isvector(y) || numel(x) ~= numel(y) || isempty(x)
    error('Nelson:comet:invalidXAndY', 'X and Y must be nonempty numeric vectors with the same number of elements.');
  end
  if ~isnumeric(p) || ~isscalar(p) || ~isfinite(p) || p < 0 || p >= 1
    error('Nelson:comet:invalidP', 'P must be a scalar value in the interval [0, 1).');
  end
  x = x(:).';
  y = y(:).';
end
%=============================================================================
