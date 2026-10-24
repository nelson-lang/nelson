%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = butter(varargin)
  narginchk(2, 3);
  nargoutchk(0, 3);
  n = varargin{1};
  wn = varargin{2};
  ftype = '';
  if nargin == 3
    ftype = varargin{3};
  end
  mustBeInteger(n);
  mustBePositive(n);
  if length(wn) ~= 1
    if length(wn) ~= 2
      error('Nelson:signal:invalidCutoffFrequency', 'Cutoff frequency must be a scalar or a two-element vector.');
    end
    if ~(isempty(ftype) || strcmpi(ftype, 'bandpass') || strcmpi(ftype, 'pass') || strcmpi(ftype, 'stop'))
      error('Nelson:signal:cutoffVectorBandpassBandstop', 'Cutoff frequency must be a two-element vector for bandpass and bandstop filters.');
    end
    prototypePoles = signalButterPrototypePoles(n);
    warped = 2 * tan(pi * wn(:).' / 2);
    bandwidth = warped(2) - warped(1);
    center = sqrt(warped(1) * warped(2));
    if strcmpi(ftype, 'stop')
      analogPoles = zeros(2 * n, 1);
      for k = 1:n
        rootsPair = roots([prototypePoles(k), -bandwidth, prototypePoles(k) * center ^ 2]);
        analogPoles(2 * k - 1:2 * k) = rootsPair;
      end
      analogZeros = [1i * center * ones(n, 1); -1i * center * ones(n, 1)];
      normalizeAt = 1;
    else
      analogPoles = zeros(2 * n, 1);
      for k = 1:n
        rootsPair = roots([1, -bandwidth * prototypePoles(k), center ^ 2]);
        analogPoles(2 * k - 1:2 * k) = rootsPair;
      end
      analogZeros = zeros(n, 1);
      normalizeAt = exp(1i * 2 * atan(center / 2));
    end
    digitalPoles = (1 + analogPoles / 2) ./ (1 - analogPoles / 2);
    digitalZeros = (1 + analogZeros / 2) ./ (1 - analogZeros / 2);
    if ~strcmpi(ftype, 'stop')
      digitalZeros = [digitalZeros; -ones(n, 1)];
    end
    b = real(poly(digitalZeros));
    a = real(poly(digitalPoles));
    gain = abs(polyval(a, normalizeAt) / polyval(b, normalizeAt));
    b = b * gain;
    if nargout <= 2
      varargout{1} = b;
      if nargout > 1
        varargout{2} = a;
      end
    else
      [varargout{1}, varargout{2}, varargout{3}] = tf2zp(b, a);
    end
    return;
  end
  poles = signalButterPrototypePoles(n);
  warped = 2 * tan(pi * wn / 2);
  if ~(isempty(ftype) || strcmpi(ftype, 'low') || strcmpi(ftype, 'high'))
    error('Nelson:signal:cutoffScalarLowpassHighpass', 'Cutoff frequency must be a scalar for lowpass and highpass filters.');
  end
  if strcmpi(ftype, 'high')
    analogPoles = warped ./ poles;
    zerosArray = ones(n, 1);
    normalizeAt = -1;
  else
    analogPoles = warped * poles;
    zerosArray = -ones(n, 1);
    normalizeAt = 1;
  end
  digitalPoles = (1 + analogPoles / 2) ./ (1 - analogPoles / 2);
  b = real(poly(zerosArray));
  a = real(poly(digitalPoles));
  gain = abs(polyval(a, normalizeAt) / polyval(b, normalizeAt));
  b = b * gain;
  if nargout <= 2
    varargout{1} = b;
    if nargout > 1
      varargout{2} = a;
    end
  else
    varargout{1} = zerosArray;
    varargout{2} = digitalPoles;
    varargout{3} = gain;
  end
end
%=============================================================================
