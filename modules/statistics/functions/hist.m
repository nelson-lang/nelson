%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The graphics module carries its own hist, which shadows this one as soon as
% that module is loaded. This one is what answers without it, so the two count
% their bins the same way: see modules/graphics/functions/hist.m.
%=============================================================================
function varargout = hist(y, x)
  narginchk(1, 2);
  nargoutchk(0, 2);

  % A matrix is a set of samples, one per column, and each column is counted on
  % its own: only the trailing dimensions are folded onto columns. A vector is a
  % single sample whichever way round it is written.
  asVector = isvector(y);
  if asVector
    y = y(:);
  else
    y = y(:, :);
  end
  if isequal(size(y), [0 0])
    % [] is read as a single sample holding nothing: it answers one column of
    % zero counts.
    y = zeros(0, 1);
  end

  if ~isreal(y)
    y = real(y);
  end
  % The counting reads the values, not what the sample's type is able to hold:
  % laying the bins out on an integer sample would otherwise saturate.
  y = double(y);
  % The bins are read on the whole sample: every column is counted against the
  % same bins.
  max_val = max(y(:));
  min_val = min(y(:));

  % The bins have to be given before anything can be said about how the bars
  % look: a style standing here is refused rather than read as a bin
  % specification. A single character passes isscalar() and would count as a
  % number of bins, a longer one passes isreal() and would count as bin centres
  % sitting at the codes of its characters.
  if (nargin > 1)
    if ~(isnumeric(x) || islogical(x))
      error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Second argument');
    end
  end

  % The centres are handed back the way they were given, and the ones built here
  % count as a row. A matrix sample hands them back transposed.
  centersGivenAsRow = true;
  if (nargin < 2)
    n = 10;
    x = binCentres(min_val, max_val, n);
  elseif (isscalar(x))
    n = x;
    if (n <= 0)
      error('Nelson:validators:mustBePositive', '%s must be positive.', 'Second argument');
    end
    x = binCentres(min_val, max_val, n);
  elseif (isreal(x))
    if (isvector(x))
      centersGivenAsRow = isrow(x);
      x = x(:);
    end
    x = sort(x);
  else
    error('Nelson:hist:secondArgumentNotScalarVector', 'Second argument must be a scalar or vector.');
  end

  x = double(x);
  freq = computesFreq(x, y);

  if (nargout == 0)
    % Drawing is the graphics module's side of hist; without it only the counts
    % can be answered, and saying so beats letting bar go missing on its own.
    if isempty(which('bar'))
      error('Nelson:hist:graphicsModuleRequired', 'Drawing a histogram requires the %s module. Ask for the counts instead: n = hist(...).', 'graphics');
    end
    bar(x, freq);
    return
  end

  if ~asVector
    varargout{1} = freq;
  else
    varargout{1} = freq';
  end

  if nargout > 1
    % x is held as a column whatever was given: hand it back the way it came in
    % for a vector sample, and the other way round for a matrix one.
    if (isvector(x) && (centersGivenAsRow == asVector))
      varargout{2} = x';
    else
      varargout{2} = x;
    end
  end
end
%=============================================================================
function x = binCentres(min_val, max_val, n)
  % A sample with no spread at all still has to be given somewhere to sit: lay
  % the bins one unit wide around the value so that it lands in the middle one
  % rather than collapsing every centre onto it. A sample with nothing in it is
  % counted against the unit range, so that it answers zero counts instead of
  % failing. An all-NaN sample keeps its NaN bins: the comparison below is false
  % for NaN, so it falls through to the plain range.
  if (isempty(min_val))
    min_val = 0;
    max_val = 1;
  elseif (min_val == max_val)
    min_val = min_val - floor(n / 2) - 0.5;
    max_val = max_val + ceil(n / 2) - 0.5;
  end
  x = [0.5:n]' / n;
  x = x * (max_val - min_val) + ones(size(x)) * min_val;
end
%=============================================================================
function freq = computesFreq(x, y)
  cutout = (x(1:end - 1, :) + x(2:end, :)) / 2;
  % A sample sitting exactly on a cut point belongs to the bin below it. The
  % sort below is free to hand back such a tie either way round, so the cut
  % points are pushed just above the values they must still hold instead of
  % relying on the order the sort picks.
  cutout = cutout + (abs(cutout) + realmin) * eps;
  n = size(x, 1);
  offset = 30;
  withLoopOnN = (n < offset && size(x, 2) == 1);
  if ~withLoopOnN
    if (size(cutout, 2) == 1 && size(y, 2) > 1)
      % A single set of bins shared by every column: the sort below reads the
      % cut points alongside each column of the sample.
      cutout = repmat(cutout, 1, size(y, 2));
    end
    [s, idx] = sort([y; cutout], 1);
    lenY = size(y, 1);
    histValues = cumsum(idx <= lenY);
    x1 = zeros(1, size(y, 2));
    x2 = reshape(histValues(idx > lenY), size(cutout));
    x3 = histValues(end, :) - sum(isnan(y), 1);
    histValues = [x1; x2; x3];
  else
    histValues = zeros(n + 1, size(y, 2));
    for i = 1:n - 1
      histValues(i + 1, :) = sum(y <= cutout(i), 1);
    end
    histValues(n + 1, :) = sum(~isnan(y), 1);
  end
  freq = diff(histValues);
end
%=============================================================================
