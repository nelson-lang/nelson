%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = histcounts2(varargin)
  narginchk(2, Inf);
  nargoutchk(0, 3);

  X = varargin{1};
  Y = varargin{2};
  if (~isnumeric(X) && ~islogical(X)) || (~isnumeric(Y) && ~islogical(Y))
    error('Nelson:validators:mustBeNumericOrLogical', '%s must be numeric or logical.', 'First two inputs');
  end
  if numel(X) ~= numel(Y)
    error('Nelson:elementary_functions:xYSameNumberElements', 'X and Y must have the same number of elements.');
  end
  xv = double(X(:));
  yv = double(Y(:));
  valid = ~isnan(xv) & ~isnan(yv);

  % Parse the optional positional bin specification (a number of bins or the
  % explicit edge vectors) followed by the Name-Value options.
  args = varargin(3:end);
  positional = {};
  i = 1;
  while i <= numel(args) && (isnumeric(args{i}) || islogical(args{i}))
    positional{end + 1} = args{i};
    i = i + 1;
  end
  normalization = 'count';
  while i <= numel(args)
    name = lower(char(args{i}));
    if i + 1 > numel(args)
      error('Nelson:elementary_functions:valueExpectedAfterOption', 'Value expected after option.');
    end
    value = args{i + 1};
    switch name
      case 'normalization'
        normalization = lower(char(value));
      otherwise
        error('Nelson:elementary_functions:unknownOption', 'Unknown option.');
    end
    i = i + 2;
  end

  % Determine the bin edges in each dimension. The number-of-bins and automatic
  % edges reuse the 1-D histcounts bin picker so that histcounts and histcounts2
  % agree on the edges.
  if isempty(positional)
    [~, xedges] = histcounts(xv(valid));
    [~, yedges] = histcounts(yv(valid));
  elseif numel(positional) == 1
    nb = double(positional{1});
    if isscalar(nb)
      nx = nb; ny = nb;
    elseif numel(nb) == 2
      nx = nb(1); ny = nb(2);
    else
      error('Nelson:elementary_functions:numberBinsScalarTwoElement', 'Number of bins must be a scalar or a two-element vector.');
    end
    if any([nx, ny] < 1) || any([nx, ny] ~= fix([nx, ny]))
      error('Nelson:elementary_functions:numberBinsPositiveInteger', '%s must be a positive integer scalar.', 'Number of bins');
    end
    [~, xedges] = histcounts(xv(valid), nx);
    [~, yedges] = histcounts(yv(valid), ny);
  elseif numel(positional) == 2
    xedges = double(positional{1}(:).');
    yedges = double(positional{2}(:).');
    if numel(xedges) < 2 || any(diff(xedges) <= 0) || numel(yedges) < 2 || any(diff(yedges) <= 0)
      error('Nelson:elementary_functions:binEdgesMonotonicallyIncreasingVector', 'Bin edges must be a monotonically increasing vector.');
    end
  else
    error('Nelson:elementary_functions:tooManyNumericArguments', 'Too many numeric arguments.');
  end

  nbx = numel(xedges) - 1;
  nby = numel(yedges) - 1;
  bx = binIndices(xv, xedges);
  by = binIndices(yv, yedges);

  N = zeros(nbx, nby);
  for k = 1:numel(xv)
    if valid(k) && bx(k) > 0 && by(k) > 0
      N(bx(k), by(k)) = N(bx(k), by(k)) + 1;
    end
  end

  total = sum(N(:));
  xWidths = diff(xedges);
  yWidths = diff(yedges);
  binAreas = xWidths(:) * yWidths(:).';
  switch normalization
    case 'count'
      % N unchanged.
    case 'probability'
      if total > 0
        N = N ./ total;
      end
    case 'countdensity'
      N = N ./ binAreas;
    case 'pdf'
      if total > 0
        N = N ./ (total .* binAreas);
      end
    case 'cumcount'
      N = cumsum(cumsum(N, 1), 2);
    case 'cdf'
      if total > 0
        N = cumsum(cumsum(N, 1), 2) ./ total;
      end
    otherwise
      error('Nelson:elementary_functions:unsupportedNormalizationValue', 'Unsupported ''Normalization'' value.');
  end

  varargout{1} = N;
  if nargout > 1
    varargout{2} = xedges;
  end
  if nargout > 2
    varargout{3} = yedges;
  end
end
%=============================================================================
function idx = binIndices(v, edges)
  % Bin index (or 0 when outside) for every value; the last bin is closed on
  % both ends, all other bins include their left edge only.
  nb = numel(edges) - 1;
  idx = zeros(size(v));
  for k = 1:nb
    if k < nb
      sel = v >= edges(k) & v < edges(k + 1);
    else
      sel = v >= edges(k) & v <= edges(k + 1);
    end
    idx(sel) = k;
  end
end
%=============================================================================
