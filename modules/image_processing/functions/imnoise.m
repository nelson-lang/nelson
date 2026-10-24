%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function B = imnoise(A, varargin)
  % Add noise to an image.
  %   B = imnoise(A, 'gaussian', m, v)       Gaussian white noise (mean m, variance v)
  %   B = imnoise(A, 'salt & pepper', d)     salt & pepper noise with density d
  %   B = imnoise(A, 'speckle', v)           multiplicative noise  B = A + A.*n
  %   B = imnoise(A, 'poisson')              Poisson noise
  narginchk(1, 4);
  classIn = class(A);
  a = im2double(A);
  type = 'gaussian';
  if nargin >= 2
    type = lower(convertStringsToChars(varargin{1}));
  end
  switch type
    case 'gaussian'
      m = paramOr(varargin, 2, 0);
      v = paramOr(varargin, 3, 0.01);
      b = a + sqrt(v) * randn(size(a)) + m;
      b = clampUnit(b);
    case {'salt & pepper', 'salt&pepper'}
      d = paramOr(varargin, 2, 0.05);
      b = a;
      r = rand(size(a));
      b(r < d / 2) = 0;
      b(r >= d / 2 & r < d) = 1;
    case 'speckle'
      v = paramOr(varargin, 2, 0.04);
      b = a + a .* (sqrt(12 * v) * (rand(size(a)) - 0.5));
      b = clampUnit(b);
    case 'poisson'
      % Poisson noise is applied to the photon counts; scale [0,1] intensities
      % by a large factor, draw, and scale back.
      scale = 1e12;
      b = poissrnd(a * scale) / scale;
      b = clampUnit(b);
    otherwise
      error('Nelson:imnoise:unknownNoiseType', 'Unknown noise type: %s.', type);
  end
  B = castToClass(b, classIn);
end
%=============================================================================
function value = paramOr(args, index, defaultValue)
  if numel(args) >= index && ~isempty(args{index})
    value = args{index};
  else
    value = defaultValue;
  end
end
%=============================================================================
function b = clampUnit(b)
  b = max(0, min(1, b));
end
%=============================================================================
function B = castToClass(b, classIn)
  switch classIn
    case 'double'
      B = b;
    case 'single'
      B = single(b);
    case 'uint8'
      B = im2uint8(b);
    case 'uint16'
      B = im2uint16(b);
    case 'int16'
      B = im2int16(b);
    otherwise
      B = b;
  end
end
%=============================================================================
