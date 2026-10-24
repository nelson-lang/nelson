%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = randsample(varargin)
  narginchk(2, 4);
  nargoutchk(0, 1);

  population = varargin{1};
  k = positiveInteger(varargin{2}, sprintf(_('%s must be a positive integer scalar.'), 'K'));
  replacement = false;
  weights = [];
  if nargin >= 3
    replacement = logicalScalar(varargin{3}, 'Replacement');
  end
  if nargin == 4
    if ~replacement
      error('Nelson:statistics:weightedSamplingRequiresReplacement', 'Weighted sampling requires replacement.');
    end
    weights = varargin{4};
  end

  values = populationValues(population);
  n = length(values);
  if ~replacement && k > n
    error('Nelson:statistics:kExceedPopulationSizeWithout', 'K must not exceed the population size without replacement.');
  end
  idx = sampleIndices(n, k, replacement, weights);
  y = values(idx);
  y = y(:);
end
%=============================================================================
function values = populationValues(population)
  if isscalar(population) && isnumeric(population)
    n = positiveInteger(population, sprintf(_('%s must be a positive integer scalar.'), 'N'));
    values = (1:n)';
  else
    if isempty(population)
      error('Nelson:statistics:populationEmpty', 'Population must not be empty.');
    end
    values = population(:);
  end
end
%=============================================================================
function idx = sampleIndices(n, k, replacement, weights)
  if isempty(weights)
    if replacement
      idx = randi(n, k, 1);
    else
      [~, order] = sort(rand(n, 1));
      idx = order(1:k);
    end
    return
  end
  w = weights(:);
  if length(w) ~= n || any(w < 0) || sum(w) <= 0
    error('Nelson:statistics:weightsOneValuePerPopulation', 'Weights must be a nonnegative vector with one value per population element.');
  end
  cdf = cumsum(w ./ sum(w));
  u = rand(k, 1);
  idx = ones(k, 1);
  for i = 1:k
    idx(i) = find(u(i) <= cdf, 1);
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
