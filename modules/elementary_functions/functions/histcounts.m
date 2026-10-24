%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = histcounts(varargin)
  narginchk(1, Inf);
  nargoutchk(0, 3);

  X = varargin{1};
  if ~isnumeric(X) && ~islogical(X)
    error('Nelson:validators:mustBeNumericOrLogical', '%s must be numeric or logical.', 'First input');
  end
  xv = double(X(:));
  finite = xv(~isnan(xv));

  % Parse the optional positional argument (number of bins or edges) and the
  % Name-Value options (BinMethod, Normalization, NumBins, BinLimits).
  nbins = [];
  edges = [];
  binMethod = 'auto';
  normalization = 'count';
  binLimits = [];
  args = varargin(2:end);
  i = 1;
  if ~isempty(args) && (isnumeric(args{1}) || islogical(args{1}))
    a2 = args{1};
    if isscalar(a2)
      nbins = double(a2);
      if nbins < 1 || nbins ~= fix(nbins)
        error('Nelson:elementary_functions:numberBinsPositiveInteger', '%s must be a positive integer scalar.', 'Number of bins');
      end
    else
      edges = double(a2(:).');
      if numel(edges) < 2 || any(diff(edges) <= 0)
        error('Nelson:elementary_functions:binEdgesMonotonicallyIncreasingVector', 'Bin edges must be a monotonically increasing vector.');
      end
    end
    i = 2;
  end
  while i <= numel(args)
    name = lower(char(args{i}));
    if i + 1 > numel(args)
      error('Nelson:elementary_functions:valueExpectedAfterOption', 'Value expected after option.');
    end
    value = args{i + 1};
    switch name
      case 'binmethod'
        binMethod = lower(char(value));
      case 'normalization'
        normalization = lower(char(value));
      case 'numbins'
        nbins = double(value);
      case 'binlimits'
        binLimits = double(value);
      otherwise
        error('Nelson:elementary_functions:unknownOption', 'Unknown option.');
    end
    i = i + 2;
  end

  if isempty(edges)
    dataForEdges = finite;
    if ~isempty(binLimits)
      dataForEdges = dataForEdges(dataForEdges >= binLimits(1) & dataForEdges <= binLimits(2));
    end
    if ~isempty(nbins)
      if ~isempty(binLimits)
        % Hard bin limits: uniform edges spanning exactly the limits.
        lo = binLimits(1); hi = binLimits(2);
        edges = [lo + (0:nbins - 1) .* ((hi - lo) / nbins), hi];
      elseif isempty(dataForEdges)
        edges = 0:nbins;
      else
        % Specified number of bins: place edges on a 'nice' grid that may
        % extend beyond [min,max] while keeping exactly nbins bins.
        edges = numBinsEdges(min(dataForEdges), max(dataForEdges), nbins);
      end
    elseif strcmp(binMethod, 'integers')
      edges = integerEdges(dataForEdges);
    else
      edges = autoEdges(dataForEdges);
    end
  end

  nb = numel(edges) - 1;
  bin = zeros(size(X));
  b = zeros(size(xv));
  for k = 1:nb
    if k < nb
      sel = xv >= edges(k) & xv < edges(k + 1);
    else
      sel = xv >= edges(k) & xv <= edges(k + 1);
    end
    b(sel) = k;
  end
  bin(:) = b;

  N = zeros(1, nb);
  for k = 1:nb
    N(k) = sum(b == k);
  end

  total = sum(N);
  binWidths = diff(edges);
  switch normalization
    case 'count'
      % N unchanged.
    case 'probability'
      if total > 0
        N = N ./ total;
      end
    case 'countdensity'
      N = N ./ binWidths;
    case 'pdf'
      if total > 0
        N = N ./ (total .* binWidths);
      end
    case 'cumcount'
      N = cumsum(N);
    case 'cdf'
      if total > 0
        N = cumsum(N) ./ total;
      end
    otherwise
      error('Nelson:elementary_functions:unsupportedNormalizationValue', 'Unsupported ''Normalization'' value.');
  end

  varargout{1} = N;
  if nargout > 1
    varargout{2} = edges;
  end
  if nargout > 2
    varargout{3} = bin;
  end
end
%=============================================================================
function edges = integerEdges(finite)
  % 'integers' bin method: unit-width bins centered on integer values.
  if isempty(finite)
    edges = [-0.5 0.5];
    return;
  end
  minx = min(finite);
  maxx = max(finite);
  firstEdge = floor(minx) - 0.5;
  lastEdge = ceil(maxx) + 0.5;
  edges = firstEdge:1:lastEdge;
end
%=============================================================================
function edges = autoEdges(finite)
  % Automatic bin selection. Reproduces the expected 'integers' rule exactly
  % for integer-valued data and approximates Scott's rule otherwise.
  if isempty(finite)
    edges = [0 1];
    return;
  end
  minx = min(finite);
  maxx = max(finite);
  xrange = maxx - minx;
  if minx == maxx
    edges = [minx - 0.5, minx + 0.5];
    return;
  end
  if all(finite == round(finite)) && xrange <= 50
    % Integer rule: unit-width bins centered on integers.
    bw = max(1, ceil(xrange / 100));
    firstEdge = floor(minx) - 0.5;
    lastEdge = ceil(maxx) + 0.5;
    edges = firstEdge:bw:lastEdge;
    if edges(end) < lastEdge
      edges(end + 1) = edges(end) + bw;
    end
  else
    % Scott's rule with a 'nice' bin width.
    n = numel(finite);
    sd = std(finite);
    if sd > 0
      rbw = 3.5 * sd / (n ^ (1 / 3));
    else
      rbw = xrange / 10;
    end
    edges = binPicker(minx, maxx, rbw);
  end
end
%=============================================================================
function edges = binPicker(minx, maxx, rawBinWidth)
  p = 10 ^ floor(log10(rawBinWidth));
  rel = rawBinWidth / p;
  % 'nice' bin widths 1/2/3/5/10 x 10^k, matching expected behavior binpicker
  % thresholds (the 3x step was missing, so 0.399 rounded to 0.5 instead of 0.3).
  if rel < 1.5
    bw = 1 * p;
  elseif rel < 2.5
    bw = 2 * p;
  elseif rel < 4
    bw = 3 * p;
  elseif rel < 7.5
    bw = 5 * p;
  else
    bw = 10 * p;
  end
  leftEdge = min(bw * floor(minx / bw), minx);
  nb = max(1, ceil((maxx - leftEdge) / bw));
  edges = leftEdge + (0:nb) * bw;
end
%=============================================================================
function edges = numBinsEdges(xmin, xmax, nbins)
  % Edge selection when a number of bins is requested. Mirrors the reference
  % bin picker for the specified-number-of-bins branch: the bin width is
  % derived from (xmax-xmin)/nbins, edges are anchored on a 'nice' grid that
  % may extend past [xmin,xmax], and exactly nbins bins are produced.
  xscale = max(abs([xmin, xmax]));
  xrange = xmax - xmin;
  rawBinWidth = max(xrange / nbins, eps(xscale));
  if xrange > max(sqrt(eps) * xscale, realmin)
    powOfTen = 10 .^ floor(log10(rawBinWidth));
    relSize = rawBinWidth / powOfTen;
    binWidth = powOfTen * floor(relSize);
    leftEdge = max(min(binWidth * floor(xmin ./ binWidth), xmin), -realmax);
    if nbins > 1
      % Refine the bin width so that xmax falls into the rightmost bin.
      ll = (xmax - leftEdge) / nbins;
      ul = (xmax - leftEdge) / (nbins - 1);
      p10 = 10 ^ floor(log10(ul - ll));
      binWidth = p10 * ceil(ll ./ p10);
    end
    rightEdge = min(max(leftEdge + nbins .* binWidth, xmax), realmax);
  else
    % Nearly constant data: cover the value with an integer-ish range.
    binRange = max(1, ceil(nbins * eps(xscale)));
    leftEdge = floor(2 * (xmin - binRange ./ 4)) / 2;
    rightEdge = ceil(2 * (xmax + binRange ./ 4)) / 2;
    binWidth = (rightEdge - leftEdge) ./ nbins;
  end
  if ~isfinite(binWidth)
    edges = linspace(leftEdge, rightEdge, nbins + 1);
  else
    edges = [leftEdge, leftEdge + (1:nbins - 1) .* binWidth, rightEdge];
  end
end
%=============================================================================
