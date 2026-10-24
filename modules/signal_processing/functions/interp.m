%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = interp(varargin)
  narginchk(2, 4);
  nargoutchk(0, 1);
  x = varargin{1};
  r = varargin{2};
  if nargin >= 3
    n = varargin{3};
  else
    n = 4;
  end
  if nargin == 4
    alpha = varargin{4};
  else
    alpha = 0.5;
  end

  mustBeNumeric(x);
  if isempty(x) || ~isvector(x)
    error('Nelson:signal:invalidInputSignal', 'Input signal must be a nonempty vector.');
  end
  mustBeNumeric(r);
  mustBeScalarOrEmpty(r);
  mustBeFinite(r);
  mustBeInteger(r);
  mustBePositive(r);
  mustBeNumeric(n);
  mustBeScalarOrEmpty(n);
  mustBeFinite(n);
  mustBeInteger(n);
  mustBeGreaterThanOrEqual(n, 1);
  mustBeNumeric(alpha);
  mustBeScalarOrEmpty(alpha);
  mustBeFinite(alpha);
  mustBeGreaterThan(alpha, 0);
  mustBeLessThanOrEqual(alpha, 1);

  if r == 1
    y = x;
    return;
  end
  if length(x) < 2 * n + 1
    error('Nelson:signal:interp:sequenceTooShort', 'Length of data sequence must be at least two times the filter length plus one.');
  end

  h = signalIntfilt(r, n, alpha);
  y = interpolateWithLinearEdges(x, h, r, n);
end
%=============================================================================
function y = interpolateWithLinearEdges(x, h, r, n)
  wasRow = size(x, 1) == 1;
  xv = x(:).';
  left = xv(1) - (n:-1:1) * (xv(2) - xv(1));
  right = xv(end) + (1:n) * (xv(end) - xv(end - 1));
  extended = [left, xv, right];
  filtered = upfirdn(extended, h, r, 1);
  first = 2 * n * r;
  y = filtered(first:first + length(xv) * r - 1);
  if ~wasRow
    y = y.';
  end
end
%=============================================================================
