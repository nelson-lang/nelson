%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = cheby2(varargin)
  narginchk(3, 4);
  nargoutchk(0, 3);
  n = varargin{1};
  rs = varargin{2};
  wn = varargin{3};
  ftype = '';
  if nargin == 4
    ftype = varargin{4};
  end
  mustBeInteger(n);
  mustBePositive(n);
  [prototypeZeros, prototypePoles] = signalCheby2Prototype(n, rs);
  missingZeros = n - length(prototypeZeros);
  if length(wn) ~= 1
    if length(wn) ~= 2
      error('Nelson:signal:invalidCutoffFrequency', 'Cutoff frequency must be a scalar or a two-element vector.');
    end
    if ~(isempty(ftype) || strcmpi(ftype, 'bandpass') || strcmpi(ftype, 'pass') || strcmpi(ftype, 'stop'))
      error('Nelson:signal:cutoffVectorBandpassBandstop', 'Cutoff frequency must be a two-element vector for bandpass and bandstop filters.');
    end
    warped = 2 * tan(pi * wn(:).' / 2);
    bandwidth = warped(2) - warped(1);
    center = sqrt(warped(1) * warped(2));
    analogPoles = zeros(2 * n, 1);
    analogZeros = [];
    if strcmpi(ftype, 'stop')
      for k = 1:n
        rootsPair = roots([prototypePoles(k), -bandwidth, prototypePoles(k) * center ^ 2]);
        analogPoles(2 * k - 1:2 * k) = rootsPair;
      end
      for k = 1:length(prototypeZeros)
        analogZeros = [analogZeros; roots([prototypeZeros(k), -bandwidth, prototypeZeros(k) * center ^ 2])];
      end
      analogZeros = [analogZeros; 1i * center * ones(missingZeros, 1); -1i * center * ones(missingZeros, 1)];
      normalizeAt = 1;
    else
      for k = 1:n
        rootsPair = roots([1, -bandwidth * prototypePoles(k), center ^ 2]);
        analogPoles(2 * k - 1:2 * k) = rootsPair;
      end
      for k = 1:length(prototypeZeros)
        analogZeros = [analogZeros; roots([1, -bandwidth * prototypeZeros(k), center ^ 2])];
      end
      analogZeros = [analogZeros; zeros(missingZeros, 1)];
      digitalZerosAtInfinity = -ones(missingZeros, 1);
      normalizeAt = exp(1i * 2 * atan(center / 2));
    end
    digitalPoles = (1 + analogPoles / 2) ./ (1 - analogPoles / 2);
    digitalZeros = (1 + analogZeros / 2) ./ (1 - analogZeros / 2);
    if ~strcmpi(ftype, 'stop')
      digitalZeros = [digitalZeros; digitalZerosAtInfinity];
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
      varargout{1} = digitalZeros;
      varargout{2} = digitalPoles;
      varargout{3} = gain;
    end
    return;
  end
  warped = 2 * tan(pi * wn / 2);
  if ~(isempty(ftype) || strcmpi(ftype, 'low') || strcmpi(ftype, 'high'))
    error('Nelson:signal:cutoffScalarLowpassHighpass', 'Cutoff frequency must be a scalar for lowpass and highpass filters.');
  end
  if strcmpi(ftype, 'high')
    analogPoles = warped ./ prototypePoles;
    analogZeros = warped ./ prototypeZeros;
    analogZeros = [analogZeros; zeros(missingZeros, 1)];
    normalizeAt = -1;
  else
    analogPoles = warped * prototypePoles;
    analogZeros = warped * prototypeZeros;
    digitalZerosAtInfinity = -ones(missingZeros, 1);
    normalizeAt = 1;
  end
  digitalPoles = (1 + analogPoles / 2) ./ (1 - analogPoles / 2);
  if strcmpi(ftype, 'high')
    digitalZeros = (1 + analogZeros / 2) ./ (1 - analogZeros / 2);
  else
    digitalZeros = (1 + analogZeros / 2) ./ (1 - analogZeros / 2);
    digitalZeros = [digitalZeros; digitalZerosAtInfinity];
  end
  b = real(poly(digitalZeros));
  a = real(poly(digitalPoles));
  gain = real(polyval(a, normalizeAt) / polyval(b, normalizeAt));
  b = b * gain;
  if nargout <= 2
    varargout{1} = b;
    if nargout > 1
      varargout{2} = a;
    end
  else
    varargout{1} = digitalZeros;
    varargout{2} = digitalPoles;
    varargout{3} = gain;
  end
end
%=============================================================================
