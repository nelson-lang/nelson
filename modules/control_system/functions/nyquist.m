%=============================================================================
% Copyright (c) 2017 Oktober Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = nyquist(varargin)
  % nyquist(sys)
  % nyquist(sys, w)
  % [re, im, w] = nyquist(...)
  
  narginchk(1, 2);
  nargoutchk(0, 3);
  
  sys = varargin{1};
  if ~islti(sys)
    error(_('LTI model expected.'));
  end
  
  sys = tf(sys);
  if ~issiso(sys)
    error(_('SISO LTI model expected.'));
  end
  numerator = sys.Numerator{1, 1};
  denominator = sys.Denominator{1, 1};
  Ts = sys.Ts;
  L = 500;  % Number of frequency elements - Need to be 500 for the nyquist plot
  N = length(denominator);  % Number of denomerators
  
  % Check if there is any input
  if(nargin > 1)
    W = varargin{2};
    isCellW = (iscell(W) && length(W) == 2) && isscalar(W{1}) && isscalar(W{2}) && isnumeric(W{1}) && isnumeric(W{2}); 
    isVectorW = isnumeric(W) && isvector(W);
    haveW = isCellW || isVectorW; 
    if (haveW)
      if isVectorW
        w = W;
      else
        w1 = W{1};
        w2 = W{2};
        w = logspace(log10(w1), log10(w2), L); % Angular frequencies
      end
    else
      error('Invalid parameter w.');
    end
  else
    zeroPole = [roots(numerator(:)); roots(denominator(:))];
    if isdt(sys)
      zeroPoleIndex = find(abs(zeroPole-1.0) > norm(zeroPole)*eps & abs(zeroPole) > norm(zeroPole)*eps);
      zeroPole = zp(zeroPoleIndex);
      zeroPole = min(abs(log(zeroPole)/(2*pi*sys.Ts)));
      wmin = floor(log10(zeroPole));
      wmax = log10(1/(2*sys.Ts));	% Nyquist
    else
      zeroPoleIndex = find (abs(zeroPole) > norm(zeroPole)*eps);			
      zeroPole = zeroPole(zeroPoleIndex);
      wmin = min(abs(zeroPole));
      wmin = floor(log10(wmin));
      wmax = max(abs(zeroPole));
      wmax = ceil(log10(wmax));
    end
    
    if abs(denominator(end)) < norm(denominator) * eps
      wmin = wmin - 0.5;
      if isct(sys)
        wmax = wmax + 2.0;
      end
    else
      wmin = wmin - 2.0;
      if isct(sys)
        wmax = wmax + 2.0;
      end
    end
    w = logspace(wmin, wmax, L);
  end
  
  % Numerator and denomerator need to be the same length
  if(length(numerator) > length(denominator))
    denominator = [zeros(1, size(numerator, 2) - size(denominator, 2)) denominator];
  elseif(length(numerator) < length(denominator))
    numerator = [zeros(1, size(denominator, 2) - size(numerator, 2)) numerator];
  end
  
  L = length(w);
  % Evaluate transfer function
  H = zeros(1, L);
  h = Ts;
  delay = 0;
  if(Ts > 0) % Discrete model
    for k = 1 : L
      H(k) = (numerator*fliplr((exp(1i*w(k)*h)).^(0 : N-1)).')/(denominator*fliplr((exp(1i*w(k)*h)).^(0 : N-1)).')*exp(-delay*exp(1i*w(k)*h));
    end
  else
    for k = 1 : L
      H(k) = (numerator*fliplr((1i*w(k)).^(0 : N-1)).')/(denominator*fliplr((1i*w(k)).^(0 : N-1)).')*exp(-delay*1i*w(k));
    end
  end
  if nargout == 0
    nyquistplot(H, i, j)
  else
    varargout{1} = reshape(real(H)', 1, 1, length(H));
    if nargout > 1
      varargout{2} = reshape(imag(H)', 1, 1, length(H));
    end
    if nargout > 2
      varargout{3} = w';
    end
  end
end
%=============================================================================
function nyquistplot(H, i, j)
  c = [0 0 1];
  ax = gca();
  cla(ax);
  hold on
  plot(ax, real(H), imag(H), '-', 'Color', c);
  plot(ax, real(H), -imag(H), '-', 'Color', c);
  [X, Y] = computesArrowsPositions(H);
  if ~isnan(X)
    scatter(ax, X, Y, '<', 'CData', c, 'MarkerFaceColor', c, 'LineWidth', .5);
    scatter(ax, X, -Y , '>', 'CData', c, 'MarkerFaceColor', c, 'LineWidth', .5);
  end
  scatter(ax, -1, 0 , 'Marker', '+', 'MarkerEdgeColor', 'r', 'LineWidth', 1.5);
  if (min(real(H)) > - 1)
    ax.XLim = [min(real(H) - 2 ), ax.XLim(2)];
  end
  hold off
  title(_('Nyquist Diagram'));
  xlabel(_('Real axis'));
  ylabel(_('Imaginary axis')); 
end
%=============================================================================
function [X, Y] = computesArrowsPositions(H)
  X = NaN;
  Y = NaN;
  realValues = [real(H), nan, real(H)];
  imagValues = [imag(H), nan, -imag(H)];
  if (length(realValues) >= 2)
    realMean = mean([min(realValues), max(realValues)]);
    realLess = find(realValues <= realMean);
    index = realLess(1);
    if (realLess(1) == 1)
      index = realLess(end);
    end
    if (index + 1) > length(realValues)
      index = length(realValues) - 2;
    end
    if (abs(imagValues(index)) < max(abs(imagValues)) / 5)
      index = floor(index / 2);
    end
    index = index - 1;
    X = realValues(index);
    Y = imagValues(index);
  end
end
%=============================================================================
