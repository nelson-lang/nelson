%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [bootstat, bootsam] = bootstrp(varargin)
  narginchk(3, Inf);
  nargoutchk(0, 2);

  nboot = positiveInteger(varargin{1}, sprintf(_('%s must be a positive integer scalar.'), 'NBOOT'));
  bootfun = varargin{2};
  args = varargin(3:end);
  firstNameValue = findFirstNameValue(args);
  dataArgs = args(1:firstNameValue - 1);
  nv = args(firstNameValue:end);

  if ~(isempty(bootfun) || isa(bootfun, 'function_handle'))
    error('Nelson:statistics:BOOTFUNEmptyFunctionHandle', 'BOOTFUN must be empty or a function handle.');
  end
  if isempty(dataArgs)
    error('Nelson:statistics:leastOneDataArgumentRequired', 'At least one %s is required.', 'data argument');
  end
  if rem(length(nv), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end

  opts = parseOptions(nv);
  [n, sampleMask, dataArgs] = validateData(dataArgs);
  bootsam = zeros(n, nboot);

  if isempty(bootfun)
    bootstat = [];
    for b = 1:nboot
      bootsam(:, b) = sampleIndices(n, opts.Weights);
    end
    return
  end

  firstIndices = sampleIndices(n, opts.Weights);
  bootsam(:, 1) = firstIndices;
  firstArgs = makeBootstrapArgs(dataArgs, sampleMask, firstIndices);
  firstStat = statisticRow(bootfun, firstArgs);
  bootstat = zeros(nboot, length(firstStat));
  bootstat(1, :) = firstStat;

  for b = 2:nboot
    indices = sampleIndices(n, opts.Weights);
    bootsam(:, b) = indices;
    bootArgs = makeBootstrapArgs(dataArgs, sampleMask, indices);
    bootstat(b, :) = statisticRow(bootfun, bootArgs);
  end
end
%=============================================================================
function idx = findFirstNameValue(args)
  idx = length(args) + 1;
  for k = 1:length(args)
    if isTextScalar(args{k})
      idx = k;
      return
    end
  end
end
%=============================================================================
function opts = parseOptions(nv)
  opts = struct('Weights', [], 'Options', []);
  for k = 1:2:length(nv)
    name = optionName(nv{k});
    value = nv{k + 1};
    switch name
      case 'weights'
        opts.Weights = value;
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
function name = optionName(value)
  if ~isTextScalar(value)
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  text = lower(char(value));
  names = {'weights', 'options'};
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
function [n, sampleMask, dataArgs] = validateData(dataArgs)
  n = [];
  sampleMask = false(1, length(dataArgs));
  for k = 1:length(dataArgs)
    value = dataArgs{k};
    if length(dataArgs) == 1 && isvector(value) && ~isscalar(value)
      dataArgs{k} = value(:);
    end
    if ~isscalar(dataArgs{k})
      rows = size(dataArgs{k}, 1);
      if isempty(n)
        n = rows;
      elseif rows ~= n
        error('Nelson:statistics:nonscalarDataArgumentsSameNumber', '%s must have the same number of rows.', 'Nonscalar data arguments');
      end
      sampleMask(k) = true;
    end
  end
  if isempty(n)
    error('Nelson:statistics:leastOneNonscalarDataArgument', 'At least one %s is required.', 'nonscalar data argument');
  end
end
%=============================================================================
function args = makeBootstrapArgs(dataArgs, sampleMask, indices)
  args = dataArgs;
  for k = 1:length(args)
    if sampleMask(k)
      args{k} = args{k}(indices, :);
    end
  end
end
%=============================================================================
function indices = sampleIndices(n, weights)
  if isempty(weights)
    indices = randi(n, n, 1);
    return
  end
  w = weights(:);
  if length(w) ~= n || any(w < 0) || sum(w) <= 0
    error('Nelson:statistics:weightsNonnegativeVectorOneValue', 'Weights must be a nonnegative vector with one value per observation.');
  end
  cdf = cumsum(w ./ sum(w));
  u = rand(n, 1);
  indices = ones(n, 1);
  for i = 1:n
    indices(i) = find(u(i) <= cdf, 1);
  end
end
%=============================================================================
function row = statisticRow(bootfun, args)
  value = feval(bootfun, args{:});
  row = value(:)';
end
%=============================================================================
function value = positiveInteger(value, message)
  if ~isscalar(value) || value <= 0 || floor(value) ~= value
    error('Nelson:statistics:mustBePositiveInteger', message);
  end
end
%=============================================================================
function tf = isTextScalar(value)
  tf = ischar(value) || isStringScalar(value);
end
%=============================================================================
