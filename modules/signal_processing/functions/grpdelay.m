%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = grpdelay(varargin)
  narginchk(1, 5);
  nargoutchk(0, 2);
  b = varargin{1};
  a = 1;
  if nargin >= 2
    a = varargin{2};
  end
  if nargin == 5 && ~(ischar(varargin{4}) || isstring(varargin{4}))
    error('Nelson:signal:grpdelay:invalidArgumentOrder', 'Frequency range must be specified before sampling frequency.');
  end
  if ismatrix(b) && ~isvector(b) && size(b, 2) == 6 && (nargin == 1 || isscalar(a))
    [b, a] = sos2tf(b);
  end
  [~, w] = freqz(varargin{:});
  fs = [];
  for idx = 3:nargin
    value = varargin{idx};
    if ~(ischar(value) || isstring(value)) && idx > 3
      fs = value;
    end
  end
  if isempty(fs)
    evalw = w;
  else
    evalw = 2 * pi * w / double(fs);
  end
  gd = computeGroupDelay(b, a, evalw);
  if nargout == 0
    plot(w, gd);
  else
    varargout{1} = gd;
    if nargout > 1
      varargout{2} = w;
    end
  end
end
%=============================================================================
function gd = computeGroupDelay(b, a, w)
  b = b(:).';
  a = a(:).';
  kb = 0:length(b) - 1;
  ka = 0:length(a) - 1;
  z = exp(-1i * w(:));
  responseB = zeros(size(z));
  responseA = zeros(size(z));
  derivativeB = zeros(size(z));
  derivativeA = zeros(size(z));
  for idx = 1:length(b)
    zk = z .^ kb(idx);
    responseB = responseB + b(idx) * zk;
    derivativeB = derivativeB + kb(idx) * b(idx) * zk;
  end
  for idx = 1:length(a)
    zk = z .^ ka(idx);
    responseA = responseA + a(idx) * zk;
    derivativeA = derivativeA + ka(idx) * a(idx) * zk;
  end
  gd = real(derivativeB ./ responseB - derivativeA ./ responseA);
  tolerance = 100 * eps() * max(1, max(abs([responseB; responseA])));
  singular = abs(responseB) <= tolerance | abs(responseA) <= tolerance | ~isfinite(gd);
  if any(singular)
    gd = replaceSingularGroupDelay(gd, singular);
  end
end
%=============================================================================
function gd = replaceSingularGroupDelay(gd, singular)
  finiteIndex = find(~singular & isfinite(gd));
  if isempty(finiteIndex)
    gd(:) = 0;
    return;
  end
  badIndex = find(singular | ~isfinite(gd));
  for idx = badIndex(:).'
    previous = finiteIndex(finiteIndex < idx);
    next = finiteIndex(finiteIndex > idx);
    if ~isempty(previous) && ~isempty(next)
      gd(idx) = (gd(previous(end)) + gd(next(1))) / 2;
    elseif ~isempty(previous)
      gd(idx) = gd(previous(end));
    else
      gd(idx) = gd(next(1));
    end
  end
end
%=============================================================================
