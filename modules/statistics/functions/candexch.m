%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function rlist = candexch(varargin)
  narginchk(2, Inf);
  nargoutchk(0, 1);

  C = varargin{1};
  nrows = positiveInteger(varargin{2}, sprintf(_('%s must be a positive integer scalar.'), 'NROWS'));
  if ~isnumeric(C) || ndims(C) ~= 2 || isempty(C)
    error('Nelson:statistics:cNonemptyNumericMatrix', '%s must be a nonempty numeric matrix.', 'C');
  end
  if size(C, 1) == 0 || size(C, 2) == 0
    error('Nelson:statistics:cNonemptyNumericMatrix', '%s must be a nonempty numeric matrix.', 'C');
  end
  if rem(nargin - 2, 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end

  opts = parseOptions(varargin(3:end), size(C, 2));
  if opts.AvoidDuplicates && nrows > size(C, 1)
    error('Nelson:statistics:NROWSExceedNumberCandidateRows', 'NROWS must not exceed the number of candidate rows when AvoidDuplicates is true.');
  end

  bestRows = [];
  bestScore = -Inf;
  for t = 1:opts.NumTries
    rows = initialRows(C, nrows, opts, t);
    [rows, score] = exchangeRows(C, rows, opts);
    if isempty(bestRows) || score > bestScore
      bestRows = rows;
      bestScore = score;
    end
  end
  rlist = bestRows(:);
end
%=============================================================================
function opts = parseOptions(args, ncols)
  opts = struct('AvoidDuplicates', false, ...
    'Display', 'on', ...
    'InitialDesign', [], ...
    'MaxIterations', 10, ...
    'Options', [], ...
    'FixedRows', [], ...
    'NumTries', 1);
  for k = 1:2:length(args)
    name = optionName(args{k});
    value = args{k + 1};
    switch name
      case 'avoidduplicates'
        opts.AvoidDuplicates = logicalScalar(value, 'AvoidDuplicates');
      case 'display'
        opts.Display = displayValue(value);
      case 'initialdesign'
        opts.InitialDesign = matrixWithColumns(value, ncols, sprintf('%s must have the same number of columns as C.', 'InitialDesign'));
      case 'maxiterations'
        opts.MaxIterations = positiveInteger(value, sprintf(_('%s must be a positive integer scalar.'), 'MaxIterations'));
      case 'options'
        if ~isempty(value) && ~isstruct(value)
          error('Nelson:statistics:optionsStructure', 'Options must be a structure.');
        end
        opts.Options = value;
      case 'fixedrows'
        opts.FixedRows = matrixWithColumns(value, ncols, sprintf('%s must have the same number of columns as C.', 'FixedRows'));
      case 'numtries'
        opts.NumTries = positiveInteger(value, sprintf(_('%s must be a positive integer scalar.'), 'NumTries'));
      otherwise
        error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
    end
  end
  if ~isempty(opts.InitialDesign) && size(opts.InitialDesign, 1) ~= 0
    opts.InitialDesignRows = [];
  end
end
%=============================================================================
function rows = initialRows(C, nrows, opts, tryIndex)
  if tryIndex == 1 && ~isempty(opts.InitialDesign)
    if size(opts.InitialDesign, 1) ~= nrows
      error('Nelson:statistics:initialDesignNROWSRows', 'InitialDesign must have NROWS rows.');
    end
    rows = findCandidateRows(C, opts.InitialDesign, opts.AvoidDuplicates);
  else
    rows = randomRows(size(C, 1), nrows, opts.AvoidDuplicates);
  end
end
%=============================================================================
function [rows, score] = exchangeRows(C, rows, opts)
  score = designScore(C, rows, opts.FixedRows);
  for iter = 1:opts.MaxIterations
    changed = false;
    for pos = 1:length(rows)
      [candidateRows, candidateScore] = bestExchange(C, rows, pos, score, opts);
      if candidateScore > score
        rows = candidateRows;
        score = candidateScore;
        changed = true;
      end
    end
    if strcmp(opts.Display, 'on')
      disp(['Iteration ', int2str(iter)]);
    end
    if ~changed
      return
    end
  end
end
%=============================================================================
function [bestRows, bestScore] = bestExchange(C, rows, pos, currentScore, opts)
  bestRows = rows;
  bestScore = currentScore;
  tol = 1e-10;
  for candidate = 1:size(C, 1)
    if opts.AvoidDuplicates && any(rows([1:pos - 1, pos + 1:end]) == candidate)
      continue
    end
    trialRows = rows;
    trialRows(pos) = candidate;
    trialScore = designScore(C, trialRows, opts.FixedRows);
    if trialScore > bestScore + tol || (isinf(bestScore) && trialScore > bestScore)
      bestRows = trialRows;
      bestScore = trialScore;
    end
  end
end
%=============================================================================
function score = designScore(C, rows, fixedRows)
  X = C(rows, :);
  if ~isempty(fixedRows)
    X = [fixedRows; X];
  end
  d = det(X' * X);
  if ~isfinite(d) || d <= 0
    score = -Inf;
  else
    score = log(d);
  end
end
%=============================================================================
function rows = randomRows(ncandidates, nrows, avoidDuplicates)
  if avoidDuplicates
    [~, order] = sort(rand(ncandidates, 1));
    rows = order(1:nrows);
  else
    rows = randi(ncandidates, nrows, 1);
  end
end
%=============================================================================
function rows = findCandidateRows(C, design, avoidDuplicates)
  rows = zeros(size(design, 1), 1);
  used = false(size(C, 1), 1);
  for i = 1:size(design, 1)
    found = 0;
    for j = 1:size(C, 1)
      if avoidDuplicates && used(j)
        continue
      end
      if isequal(C(j, :), design(i, :))
        found = j;
        break
      end
    end
    if found == 0
      error('Nelson:statistics:initialDesignRowsRowsC', 'InitialDesign rows must be rows from C.');
    end
    rows(i) = found;
    used(found) = true;
  end
end
%=============================================================================
function name = optionName(value)
  if ~isTextScalar(value)
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  text = lower(char(value));
  names = {'avoidduplicates', 'display', 'initialdesign', 'maxiterations', 'options', 'fixedrows', 'numtries'};
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
  if ~isTextScalar(value)
    error('Nelson:statistics:displayOff', 'Display must be on or off.');
  end
  text = lower(char(value));
  if strncmp('on', text, length(text))
    value = 'on';
  elseif strncmp('off', text, length(text))
    value = 'off';
  else
    error('Nelson:statistics:displayOff', 'Display must be on or off.');
  end
end
%=============================================================================
function value = matrixWithColumns(value, ncols, message)
  if isempty(value)
    return
  end
  if ~isnumeric(value) || ndims(value) ~= 2 || size(value, 2) ~= ncols
    error('Nelson:statistics:invalidValue', message);
  end
end
%=============================================================================
function value = positiveInteger(value, message)
  if ~isscalar(value) || value <= 0 || floor(value) ~= value
    error('Nelson:statistics:mustBePositiveInteger', message);
  end
end
%=============================================================================
function value = logicalScalar(value, label)
  if ~isscalar(value) || ~(isnumeric(value) || islogical(value)) || (value ~= 0 && value ~= 1)
    error('Nelson:statistics:mustBeScalarLogicalValue', '%s must be a scalar logical value.', label);
  end
  value = logical(value);
end
%=============================================================================
function tf = isTextScalar(value)
  tf = ischar(value) || isStringScalar(value);
end
%=============================================================================
