%=============================================================================
% Copyright (c) 2017 Oktober Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
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
    error('Nelson:control_system:invalidType', '%s expected.', 'LTI model');
  end

  sys = tf(sys);
  if ~issiso(sys)
    error('Nelson:control_system:invalidType', '%s expected.', 'SISO LTI model');
  end
  numerator = sys.Numerator{1, 1};
  denominator = sys.Denominator{1, 1};
  Ts = sys.Ts;
  L = 500; % Number of frequency elements - Need to be 500 for the nyquist plot
  N = length(denominator); % Number of denominators

  % Check if there is any input
  if (nargin > 1)
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
      error('Nelson:control_system:invalidParameter', 'Invalid parameter w.');
    end
  else
    zeroPole = [roots(numerator(:)); roots(denominator(:))];
    if isdt(sys)
      zeroPoleIndex = find(abs(zeroPole - 1.0) > norm(zeroPole) * eps & abs(zeroPole) > norm(zeroPole) * eps);
      zeroPole = zp(zeroPoleIndex);
      zeroPole = min(abs(log(zeroPole) / (2 * pi * sys.Ts)));
      wmin = floor(log10(zeroPole));
      wmax = log10(1 / (2 * sys.Ts)); % Nyquist
    else
      zeroPoleIndex = find(abs(zeroPole) > norm(zeroPole) * eps);
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

  % Numerator and denominator need to be the same length
  if (length(numerator) > length(denominator))
    denominator = [zeros(1, size(numerator, 2) - size(denominator, 2)) denominator];
  elseif (length(numerator) < length(denominator))
    numerator = [zeros(1, size(denominator, 2) - size(numerator, 2)) numerator];
  end

  L = length(w);
  % Evaluate transfer function
  H = zeros(1, L);
  h = Ts;
  delay = 0;
  if (Ts > 0) % Discrete model
    for k = 1:L
      H(k) = (numerator * fliplr((exp(1i * w(k) * h)) .^ (0:N - 1)).') / (denominator * fliplr((exp(1i * w(k) * h)) .^ (0:N - 1)).') * exp(-delay * exp(1i * w(k) * h));
    end
  else
    for k = 1:L
      H(k) = (numerator * fliplr((1i * w(k)) .^ (0:N - 1)).') / (denominator * fliplr((1i * w(k)) .^ (0:N - 1)).') * exp(-delay * 1i * w(k));
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
  scatter(ax, -1, 0, 'Marker', '+', 'MarkerEdgeColor', 'r', 'LineWidth', 1.5);
  if (min(real(H)) > -1)
    ax.XLim = [min(real(H) - 2), ax.XLim(2)];
  end
  % Origin reference crosshair (dotted light gray lines through Real = 0 and Imaginary = 0)
  topChildren = ax.Children;
  xLimits = ax.XLim;
  yLimits = ax.YLim;
  gridColor = [0.8, 0.8, 0.8];
  hHorizontal = plot(ax, xLimits, [0, 0], ':', 'Color', gridColor);
  hVertical = plot(ax, [0, 0], yLimits, ':', 'Color', gridColor);
  ax.XLim = xLimits;
  ax.YLim = yLimits;
  % keep the crosshair behind the response curve
  ax.Children = [topChildren; hVertical; hHorizontal];
  % Direction arrows: filled triangles oriented along the response curve, drawn
  % once the axis limits are final so the heads are shaped in screen space.
  drawDirectionArrows(ax, H, c);
  % arrow patches must not disturb the finalized limits
  ax.XLim = xLimits;
  ax.YLim = yLimits;
  box(ax, 'on');
  hold off
  hTitle = title(ax, _('Nyquist Diagram'));
  xlabel(ax, _('Real Axis'));
  ylabel(ax, _('Imaginary Axis'));
  % Center the title above the axes box with a consistent gap, expressed in
  % normalized axes units so it stays clear of the response curve regardless
  % of the data limits (the curve reaches the top of the box).
  set(hTitle, 'Units', 'normalized', 'Position', [0.5, 1.03, 0], ...
    'HorizontalAlignment', 'center', 'VerticalAlignment', 'bottom');
end
%=============================================================================
function drawDirectionArrows(ax, H, c)
  % Place one filled arrowhead on each branch of the Nyquist plot, at the
  % arc-length midpoint of the response curve, pointing along the curve tangent.
  re = real(H);
  im = imag(H);
  L = numel(re);
  if (L < 3)
    return;
  end
  segments = sqrt(diff(re) .^ 2 + diff(im) .^ 2);
  arcLength = [0, cumsum(segments)];
  total = arcLength(end);
  if (~isfinite(total) || total <= 0)
    return;
  end
  [~, k] = min(abs(arcLength - 0.5 * total));
  % keep an interior sample so the central-difference tangent is well defined
  k = max(2, min(L - 1, k));
  % tangent in the direction of increasing frequency
  tx = re(k + 1) - re(k - 1);
  ty = im(k + 1) - im(k - 1);
  % Positive-frequency branch: y = imag(H), arrow follows increasing frequency.
  drawArrowHead(ax, re(k), im(k), tx, ty, c);
  % Negative-frequency (mirror) branch: y = -imag(H). The full Nyquist contour
  % traverses this branch in the opposite sense, so the head is reversed along
  % the real axis while the imaginary component is mirrored.
  drawArrowHead(ax, re(k), -im(k), -tx, ty, c);
end
%=============================================================================
function drawArrowHead(ax, x, y, tx, ty, c)
  % Draw a filled triangular arrowhead anchored at (x, y) pointing along the
  % tangent (tx, ty). The head is shaped in pixel space so it looks correct
  % even when the two axes have different data-to-pixel scaling.
  oldUnits = ax.Units;
  ax.Units = 'pixels';
  boxPosition = ax.Position;
  ax.Units = oldUnits;
  widthPixels = boxPosition(3);
  heightPixels = boxPosition(4);
  xSpan = ax.XLim(2) - ax.XLim(1);
  ySpan = ax.YLim(2) - ax.YLim(1);
  if (widthPixels <= 0 || heightPixels <= 0 || xSpan <= 0 || ySpan <= 0)
    return;
  end
  % pixels per data unit on each axis
  scaleX = widthPixels / xSpan;
  scaleY = heightPixels / ySpan;
  % unit tangent in pixel space
  ux = tx * scaleX;
  uy = ty * scaleY;
  tangentNorm = hypot(ux, uy);
  if (tangentNorm == 0)
    return;
  end
  ux = ux / tangentNorm;
  uy = uy / tangentNorm;
  % perpendicular in pixel space
  vx = -uy;
  vy = ux;
  headLength = 13; % tip-to-base length in pixels
  headHalfWidth = 6; % half base width in pixels
  forward = 0.6 * headLength;
  backward = 0.4 * headLength;
  % triangle vertices as pixel offsets from the anchor point
  offsetX = [ux * forward, -ux * backward + vx * headHalfWidth, -ux * backward - vx * headHalfWidth];
  offsetY = [uy * forward, -uy * backward + vy * headHalfWidth, -uy * backward - vy * headHalfWidth];
  % convert the pixel offsets back to data coordinates
  xVertices = x + offsetX / scaleX;
  yVertices = y + offsetY / scaleY;
  patch(ax, xVertices, yVertices, c, 'EdgeColor', c, 'LineWidth', 0.5);
end
%=============================================================================
