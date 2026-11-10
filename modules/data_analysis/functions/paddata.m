%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function B = paddata(A, m, varargin)
  % extends A to length m along the operating dimensions with a fill value or a pattern
  narginchk(2, inf);
  [lengths, dims, opts] = padTrimOptions(A, m, true, varargin);
  B = A;
  for k = 1:numel(dims)
    d = dims(k);
    n = size(B, d);
    target = lengths(k);
    if target <= n
      continue
    end
    added = target - n;
    switch opts.Side
      case 'trailing'
        lead = 0;
      case 'leading'
        lead = added;
      otherwise
        lead = floor(added / 2);
    end
    p = (1 - lead):(n + added - lead);
    pattern = opts.Pattern;
    if n == 0
      pattern = 'constant';
    elseif n == 1 && strcmp(pattern, 'reflect')
      pattern = 'edge';
    end
    switch pattern
      case 'edge'
        map = min(max(p, 1), n);
      case 'circular'
        map = mod(p - 1, n) + 1;
      case 'flip'
        q = mod(p - 1, 2 * n);
        map = q + 1;
        map(q >= n) = 2 * n - q(q >= n);
      case 'reflect'
        q = mod(p - 1, 2 * n - 2);
        map = q + 1;
        map(q >= n) = 2 * n - 1 - q(q >= n);
      otherwise
        map = p;
        map(p < 1 | p > n) = n + 1;
        sliceSize = size(B);
        sliceSize(end + 1:d) = 1;
        sliceSize(d) = 1;
        B = cat(d, B, fillSlice(B, sliceSize, opts));
    end
    subs = repmat({':'}, 1, max(ndims(B), d));
    subs{d} = map;
    B = B(subs{:});
  end
end
%=============================================================================
function S = fillSlice(A, sliceSize, opts)
  % block of fill values with the type of A
  if iscell(A)
    if opts.HasFill
      S = repmat({opts.FillValue}, sliceSize);
    else
      S = repmat({[]}, sliceSize);
    end
  elseif isstring(A)
    if opts.HasFill
      S = repmat(string(opts.FillValue), sliceSize);
    else
      S = repmat(string(missing), sliceSize);
    end
  elseif ischar(A)
    if opts.HasFill
      S = repmat(char(opts.FillValue), sliceSize);
    else
      S = repmat(char(0), sliceSize);
    end
  elseif islogical(A)
    S = false(sliceSize);
    if opts.HasFill
      S(:) = logical(opts.FillValue);
    end
  elseif isnumeric(A)
    S = zeros(sliceSize, 'like', A);
    if opts.HasFill
      S = S + opts.FillValue;
    end
  else
    error('Nelson:padtrim:unsupportedType', 'Input type is not supported.');
  end
end
%=============================================================================
