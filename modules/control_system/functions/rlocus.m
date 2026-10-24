%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = rlocus(varargin)
  % Root locus of a SISO LTI model.
  %
  % rlocus(sys)
  % rlocus(sys, k)
  % rlocus(sys1, sys2, ...)
  % [r, k] = rlocus(sys)
  % [r, k] = rlocus(sys, k)
  %
  % The root locus is the set of closed-loop poles of feedback(k*sys, 1),
  % i.e. the roots of den + k*num, as the gain k sweeps from 0 to infinity.

  narginchk(1, inf);
  nargoutchk(0, 2);

  [systems, k] = parseArguments(varargin);

  if (nargout > 0)
    if (numel(systems) ~= 1)
      error('Nelson:control_system:invalidInputArgument', 'A single model is required when output arguments are requested.');
    end
    [r, kOut] = computeLocus(systems{1}, k);
    varargout{1} = r;
    if (nargout > 1)
      varargout{2} = kOut;
    end
    return
  end

  f = figure('Visible', 'off');
  ax = gca();
  cla(ax);
  hold(ax, 'on');
  palette = branchColors();
  colorOffset = 0;
  poleZero = [];
  lociPoints = [];
  for idx = 1:numel(systems)
    [r, kOut, origNum, den] = computeLocus(systems{idx}, k);
    [olpol, olzer] = plotLocus(ax, systems{idx}, r, origNum, den, palette, colorOffset);
    colorOffset = colorOffset + size(r, 1);
    poleZero = [poleZero; olpol(:); olzer(:)]; %#ok<AGROW>
    lociPoints = [lociPoints; r(:)]; %#ok<AGROW>
  end
  setLocusView(ax, poleZero, lociPoints);
  drawOriginCrosshair(ax);
  box(ax, 'on');
  hold(ax, 'off');
  title(ax, _('Root Locus'));
  [xlab, ylab] = axisLabels(systems{1});
  xlabel(ax, xlab);
  ylabel(ax, ylab);
  f.Visible = 'on';
end
%=============================================================================
function [systems, k] = parseArguments(args)
  systems = {};
  k = [];
  for idx = 1:numel(args)
    arg = args{idx};
    if (islti(arg))
      sys = tf(arg);
      if (~issiso(sys))
        error('Nelson:control_system:invalidType', '%s expected.', 'SISO LTI model');
      end
      systems{end + 1} = sys; %#ok<AGROW>
    elseif (isnumeric(arg) && isvector(arg) && isreal(arg))
      if (~isempty(k))
        error('Nelson:control_system:invalidInputArgument', 'Only one gain vector is allowed.');
      end
      k = sort(arg(:)');
    else
      error('Nelson:control_system:invalidType', '%s expected.', 'LTI model');
    end
  end
  if (isempty(systems))
    error('Nelson:control_system:invalidType', '%s expected.', 'LTI model');
  end
end
%=============================================================================
function [r, k, origNum, den] = computeLocus(sys, k)
  origNum = sys.Numerator{1};
  den = sys.Denominator{1};
  origNum = origNum(:)';
  den = den(:)';

  if (length(origNum) > length(den))
    error('Nelson:control_system:invalidModel', 'The model is not proper.');
  end

  % Equalise polynomial lengths (leading zeros keep the open-loop zeros intact).
  num = [zeros(1, length(den) - length(origNum)), origNum];

  n = length(den) - 1;
  if (n < 1)
    error('Nelson:control_system:invalidModel', 'The model has no dynamics.');
  end

  if (isempty(k))
    k = autoGainVector(num, den);
  else
    k = sort(k(:)');
  end

  r = assembleBranches(num, den, k, n);
end
%=============================================================================
function k = autoGainVector(num, den)
  % Pick a gain range that reveals the whole locus: branches leave the
  % open-loop poles (k = 0) and travel towards the finite zeros or to
  % infinity as k grows. The upper bound is grown geometrically until the
  % moving poles either diverge or settle onto the finite zeros.
  olpol = roots(den);
  olzer = roots(num);
  scale = max([abs([olpol; olzer]); 1]);
  % Reaching roughly a hundred times the open-loop spread is enough to show the
  % asymptotic heading of the diverging branches without blowing the gain up.
  diverge = 1e2 * scale;
  settleTol = 1e-6 * scale;

  kmax = max(scale, 1);
  previous = roots(den + kmax * num);
  for iter = 1:200
    kNext = kmax * 2;
    current = roots(den + kNext * num);
    if (max(abs(current)) > diverge)
      kmax = kNext;
      break
    end
    moved = setMovement(previous, current);
    if (moved < settleTol)
      kmax = kNext;
      break
    end
    previous = current;
    kmax = kNext;
  end

  % Sweep from a small gain (so the breakaway region near k = 0 is sampled)
  % up to kmax, keeping a generous density whatever the span turns out to be.
  logHigh = log10(kmax);
  logLow = min(logHigh - 6, -3);
  N = max(300, round(40 * (logHigh - logLow)));
  k = [0, logspace(logLow, logHigh, N)];

  % Add the exact breakaway / break-in gains (and points straddling them) so
  % the loci turn crisply where branches enter or leave the real axis instead
  % of showing a sampling corner.
  kb = breakawayGains(num, den);
  kb = kb(kb > 0 & kb <= kmax);
  if (~isempty(kb))
    k = [k, kb, kb * (1 - 1e-4), kb * (1 + 1e-4)];
  end
  k = unique(k);
end
%=============================================================================
function kb = breakawayGains(num, den)
  % Breakaway / break-in points solve d/ds(-den/num) = 0, i.e.
  % den'(s) num(s) - den(s) num'(s) = 0; the matching gain is -den/num there.
  dnum = polyder(num);
  dden = polyder(den);
  p1 = conv(dden, num);
  p2 = conv(den, dnum);
  L = max(numel(p1), numel(p2));
  p = [zeros(1, L - numel(p1)), p1] - [zeros(1, L - numel(p2)), p2];
  s = roots(p);
  if (isempty(s))
    kb = [];
    return
  end
  tol = 1e-6 * max([abs(s); 1]);
  s = real(s(abs(imag(s)) < tol));
  kb = [];
  for i = 1:numel(s)
    nv = polyval(num, s(i));
    if (abs(nv) > eps)
      kval = -polyval(den, s(i)) / nv;
      if (isfinite(kval) && kval >= 0)
        kb(end + 1) = kval; %#ok<AGROW>
      end
    end
  end
end
%=============================================================================
function moved = setMovement(previousRoots, currentRoots)
  % Maximum displacement between two pole sets once optimally matched.
  ordered = matchRoots(previousRoots(:), currentRoots(:));
  moved = max(abs(ordered - previousRoots(:)));
end
%=============================================================================
function r = assembleBranches(num, den, k, n)
  % One column of closed-loop poles per gain; rows are tracked so that each
  % row is a continuous branch (roots reordered to follow the previous gain).
  r = zeros(n, numel(k));
  previous = roots(den + k(1) * num);
  previous = sortRootsForDisplay(previous);
  r(:, 1) = previous;
  for idx = 2:numel(k)
    current = roots(den + k(idx) * num);
    current = matchRoots(previous, current(:));
    r(:, idx) = current;
    previous = current;
  end
end
%=============================================================================
function ordered = matchRoots(previous, current)
  % Greedy nearest-neighbour ordering of current roots to follow previous.
  n = numel(current);
  ordered = zeros(n, 1);
  used = false(n, 1);
  for i = 1:n
    d = abs(current - previous(i));
    d(used) = inf;
    [~, j] = min(d);
    ordered(i) = current(j);
    used(j) = true;
  end
end
%=============================================================================
function s = sortRootsForDisplay(rootsVec)
  % Order branches by real part (left to right); within a conjugate pair put the
  % upper (positive imaginary) pole first so its branch takes the first colour,
  % matching the reference plot.
  rootsVec = rootsVec(:);
  [~, order] = sortrows([real(rootsVec), -imag(rootsVec)]);
  s = rootsVec(order);
end
%=============================================================================
function [olpol, olzer] = plotLocus(ax, sys, r, origNum, den, palette, colorOffset)
  olpol = roots(den);
  olzer = roots(origNum(:));
  nColors = size(palette, 1);
  n = size(r, 1);
  % Each branch (row) is drawn with its own colour, cycling the palette; the
  % open-loop pole that starts the branch (r(row, 1)) gets that same colour.
  for row = 1:n
    c = palette(mod(colorOffset + row - 1, nColors) + 1, :);
    plot(ax, real(r(row, :)), imag(r(row, :)), '-', 'Color', c);
    plot(ax, real(r(row, 1)), imag(r(row, 1)), 'x', 'Color', c, 'LineWidth', 1.0, 'MarkerSize', 7, 'LineStyle', 'none');
  end
  % Each zero is coloured like the branch that terminates on it (the branch
  % whose high-gain end point is closest to the zero).
  endPoints = r(:, end);
  for z = 1:numel(olzer)
    [~, row] = min(abs(endPoints - olzer(z)));
    c = palette(mod(colorOffset + row - 1, nColors) + 1, :);
    plot(ax, real(olzer(z)), imag(olzer(z)), 'o', 'Color', c, 'LineWidth', 1.0, 'MarkerSize', 7, 'LineStyle', 'none');
  end
  if (isdt(sys))
    theta = linspace(0, 2 * pi, 200);
    plot(ax, cos(theta), sin(theta), ':', 'Color', [0.7 0.7 0.7]);
  end
end
%=============================================================================
function setLocusView(ax, poleZero, lociPoints)
  % Frame the view on the open-loop poles/zeros together with the portion of
  % the loci that stays near them, clipping the branches that diverge towards
  % infinity so they do not blow up the limits (as the reference plot does).
  if (isempty(poleZero))
    return
  end
  re = real(poleZero);
  im = imag(poleZero);
  base = max([max(re) - min(re), 2 * max(abs(im)), eps]);
  if (base <= 0)
    base = 1;
  end
  reach = 3 * base;
  center = (min(re) + max(re)) / 2;

  lr = real(lociPoints);
  li = imag(lociPoints);
  keep = isfinite(lr) & isfinite(li) & (abs(lr - center) <= reach) & (abs(li) <= reach);
  lrk = lr(keep);
  lik = li(keep);
  allRe = [re; lrk];

  rLo = min(allRe);
  rHi = max(allRe);
  iHiPZ = max(abs(im));
  iHiLoci = max([abs(lik); 0]);
  xSpan = max(rHi - rLo, base);
  marginR = 0.1 * xSpan;
  xLo = rLo - marginR;
  xHi = rHi + marginR;
  % Keep the origin (imaginary axis) and its crosshair in view.
  if (xLo > 0)
    xLo = -0.1 * xSpan;
  end
  if (xHi < 0)
    xHi = 0.1 * xSpan;
  end

  % Vertical extent: a vertical asymptote fed by purely real poles/zeros (e.g.
  % two real poles breaking away straight up) is not worth chasing to infinity,
  % so frame it tightly on the real-axis scale. Otherwise - branches that leave
  % the pole/zero band sideways, or asymptotes fed by complex poles - follow the
  % loci so the excursions, bulges and asymptote directions stay in view.
  beyond = max([max(lrk) - max(re), min(re) - min(lrk), 0]);
  realPolesZeros = (iHiPZ <= 1e-9 * max(base, 1));
  if (beyond <= 0.5 * base) && realPolesZeros
    yExt = 0.6 * base;
  else
    yExt = iHiLoci + 0.1 * max(xSpan, 2 * iHiLoci);
  end
  if (yExt <= 0)
    yExt = 0.5 * xSpan;
  end
  ax.XLim = [xLo, xHi];
  ax.YLim = [-yExt, yExt];
end
%=============================================================================
function drawOriginCrosshair(ax)
  % Dotted light-grey reference lines through Real = 0 and Imaginary = 0, kept
  % behind the loci (as the reference plot draws them).
  xLimits = ax.XLim;
  yLimits = ax.YLim;
  gridColor = [0.8, 0.8, 0.8];
  topChildren = ax.Children;
  hHorizontal = plot(ax, xLimits, [0, 0], ':', 'Color', gridColor);
  hVertical = plot(ax, [0, 0], yLimits, ':', 'Color', gridColor);
  ax.XLim = xLimits;
  ax.YLim = yLimits;
  ax.Children = [topChildren; hVertical; hHorizontal];
end
%=============================================================================
function [xlab, ylab] = axisLabels(sys)
  % Continuous-time axes carry the frequency unit (reciprocal time unit) as the
  % reference plot does; the discrete-time z-plane axes are dimensionless.
  if (isct(sys))
    timeUnit = 'seconds';
    try
      if (~isempty(sys.TimeUnit))
        timeUnit = sys.TimeUnit;
      end
    catch
    end
    unit = [' (', timeUnit, '^{-1})'];
    xlab = [_('Real Axis'), unit];
    ylab = [_('Imaginary Axis'), unit];
  else
    xlab = _('Real Axis');
    ylab = _('Imaginary Axis');
  end
end
%=============================================================================
function palette = branchColors()
  % Colour order used to distinguish the individual locus branches, matching
  % the blue / green / red / cyan ... sequence of the reference root-locus plot.
  palette = [0.000 0.447 0.741; ...
    0.300 0.700 0.300; ...
    0.950 0.250 0.200; ...
    0.200 0.750 0.940; ...
    0.750 0.200 0.750; ...
    0.930 0.690 0.130; ...
    0.400 0.400 0.400];
end
%=============================================================================
