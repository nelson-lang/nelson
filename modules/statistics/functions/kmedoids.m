%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [idx, C, sumd, D, midx, info] = kmedoids(varargin)
  narginchk(2, Inf);
  nargoutchk(0, 6);

  X = varargin{1};
  k = positiveInteger(varargin{2}, sprintf(_('%s must be a positive integer scalar.'), 'K'));
  if ~isnumeric(X) || ndims(X) ~= 2 || isempty(X)
    error('Nelson:statistics:xNonemptyNumericMatrix', '%s must be a nonempty numeric matrix.', 'X');
  end
  if k > size(X, 1)
    error('Nelson:statistics:kTooLarge', 'K must not exceed the number of rows in X.');
  end
  if rem(nargin - 2, 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  opts = parseOptions(varargin(3:end));
  allD = pdist2(X, X, opts.Distance);
  best = struct('idx', [], 'midx', [], 'sumd', [], 'D', [], 'total', Inf, 'iterations', 0);
  for r = 1:opts.Replicates
    start = initialMedoids(X, k, opts, r);
    result = runPam(allD, start, opts.MaxIter);
    if result.total < best.total
      best = result;
    end
  end
  idx = best.idx;
  midx = best.midx(:);
  C = X(midx, :);
  D = best.D;
  sumd = best.sumd(:);
  info = struct('Iterations', best.iterations, 'BestSum', best.total);
  if strcmp(opts.Display, 'final') || strcmp(opts.Display, 'iter')
    disp(['Best total sum of distances: ', num2str(best.total)]);
  end
end
%=============================================================================
function opts = parseOptions(args)
  opts = struct('Distance', 'sqeuclidean', 'Start', 'sample', 'Replicates', 1, 'MaxIter', 100, 'Display', 'off', 'Options', []);
  for k = 1:2:length(args)
    name = optionName(args{k});
    value = args{k + 1};
    switch name
      case 'distance'
        opts.Distance = value;
      case 'start'
        opts.Start = value;
      case 'replicates'
        opts.Replicates = positiveInteger(value, sprintf(_('%s must be a positive integer scalar.'), 'Replicates'));
      case 'maxiter'
        opts.MaxIter = positiveInteger(value, sprintf(_('%s must be a positive integer scalar.'), 'MaxIter'));
      case 'display'
        opts.Display = displayValue(value);
      case 'options'
        if ~isempty(value) && ~isstruct(value)
          error('Nelson:statistics:optionsStructure', 'Options must be a structure.');
        end
        opts.Options = value;
      otherwise
        error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
    end
  end
end
%=============================================================================
function result = runPam(allD, midx, maxIter)
  idx = [];
  for iter = 1:maxIter
    [newIdx, D] = assignMedoids(allD, midx);
    newMidx = updateMedoids(allD, newIdx, midx);
    if isequal(newMidx(:), midx(:)) && isequal(newIdx(:), idx(:))
      break
    end
    idx = newIdx;
    midx = newMidx;
  end
  [idx, D] = assignMedoids(allD, midx);
  sumd = clusterSums(idx, D, length(midx));
  result = struct('idx', idx, 'midx', midx, 'sumd', sumd, 'D', D, 'total', sum(sumd), 'iterations', iter);
end
%=============================================================================
function [idx, D] = assignMedoids(allD, midx)
  D = allD(:, midx);
  [~, idx] = min(D, [], 2);
  idx = idx(:);
end
%=============================================================================
function midx = updateMedoids(allD, idx, midx)
  for c = 1:length(midx)
    members = find(idx == c);
    if isempty(members)
      candidate = 1:size(allD, 1);
      candidate(midx) = [];
      midx(c) = candidate(randi(length(candidate)));
    else
      subD = allD(members, members);
      totals = sum(subD, 2);
      [~, pos] = min(totals);
      midx(c) = members(pos);
    end
  end
  if length(unique(midx)) < length(midx)
    midx = repairDuplicates(allD, midx);
  end
end
%=============================================================================
function midx = repairDuplicates(allD, midx)
  used = [];
  for i = 1:length(midx)
    if any(used == midx(i))
      candidates = 1:size(allD, 1);
      candidates(midx) = [];
      midx(i) = candidates(randi(length(candidates)));
    end
    used(end + 1) = midx(i);
  end
end
%=============================================================================
function sumd = clusterSums(idx, D, k)
  sumd = zeros(k, 1);
  for c = 1:k
    sumd(c) = sum(D(idx == c, c));
  end
end
%=============================================================================
function midx = initialMedoids(X, k, opts, replicate)
  start = opts.Start;
  if isnumeric(start)
    midx = numericStart(X, k, start, replicate);
  else
    if ~(ischar(start) || isStringScalar(start))
      error('Nelson:statistics:invalidStartValue', 'Invalid %s value.', 'Start');
    end
    text = lower(char(start));
    if strncmp('sample', text, length(text)) || strncmp('plus', text, length(text))
      midx = randsample(size(X, 1), k);
    else
      error('Nelson:statistics:invalidStartValue', 'Invalid %s value.', 'Start');
    end
  end
end
%=============================================================================
function midx = numericStart(X, k, start, replicate)
  if isvector(start) && length(start) == k
    midx = start(:);
  elseif ndims(start) == 2 && isequal(size(start), [k size(X, 2)])
    midx = rowsInX(X, start);
  elseif ndims(start) == 3 && size(start, 1) == k && size(start, 2) == size(X, 2)
    page = min(replicate, size(start, 3));
    midx = rowsInX(X, start(:, :, page));
  else
    error('Nelson:statistics:invalidStartValue', 'Invalid %s value.', 'Start');
  end
  if any(midx < 1) || any(midx > size(X, 1)) || any(floor(midx) ~= midx)
    error('Nelson:statistics:invalidStartValue', 'Invalid %s value.', 'Start');
  end
end
%=============================================================================
function midx = rowsInX(X, centers)
  midx = zeros(size(centers, 1), 1);
  for i = 1:size(centers, 1)
    found = 0;
    for j = 1:size(X, 1)
      if isequal(X(j, :), centers(i, :))
        found = j;
        break
      end
    end
    if found == 0
      error('Nelson:statistics:startRowsRowsX', 'Start rows must be rows from X.');
    end
    midx(i) = found;
  end
end
%=============================================================================
function name = optionName(value)
  if ~(ischar(value) || isStringScalar(value))
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  text = lower(char(value));
  names = {'distance', 'start', 'replicates', 'maxiter', 'display', 'options'};
  matches = {};
  for k = 1:length(names)
    if strncmp(names{k}, text, length(text))
      matches{end + 1} = names{k};
    end
  end
  if isempty(matches)
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  if length(matches) > 1
    error('Nelson:statistics:ambiguousNameValue', 'Ambiguous name-value argument name.');
  end
  name = matches{1};
end
%=============================================================================
function value = displayValue(value)
  if ~(ischar(value) || isStringScalar(value))
    error('Nelson:statistics:displayOffFinalIter', 'Display must be off, final, or iter.');
  end
  text = lower(char(value));
  names = {'off', 'final', 'iter'};
  for k = 1:length(names)
    if strncmp(names{k}, text, length(text))
      value = names{k};
      return
    end
  end
  error('Nelson:statistics:displayOffFinalIter', 'Display must be off, final, or iter.');
end
%=============================================================================
function value = positiveInteger(value, message)
  if ~isscalar(value) || value <= 0 || floor(value) ~= value
    error('Nelson:statistics:mustBePositiveInteger', message);
  end
end
%=============================================================================
