%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [dC, C] = candgen(varargin)
  narginchk(1, Inf);
  nargoutchk(0, 2);

  nfactors = positiveInteger(varargin{1}, sprintf(_('%s must be a positive integer scalar.'), 'NFACTORS'));
  modelspec = 'linear';
  args = varargin(2:end);
  if ~isempty(args) && ~(isTextScalar(args{1}) && isOptionName(args{1}))
    modelspec = args{1};
    args = args(2:end);
  end
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  bounds = repmat([-1 1], nfactors, 1);
  for k = 1:2:length(args)
    name = optionName(args{k});
    switch name
      case 'bounds'
        bounds = validateBounds(args{k + 1}, nfactors);
      otherwise
        error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
    end
  end
  dC = fullCandidate(bounds);
  C = x2fx(dC, modelspec);
end
%=============================================================================
function dC = fullCandidate(bounds)
  dC = zeros(1, size(bounds, 1));
  for j = 1:size(bounds, 1)
    levels = unique(bounds(j, :));
    current = dC;
    dC = [];
    for k = 1:length(levels)
      block = current;
      block(:, j) = levels(k);
      dC = [dC; block];
    end
  end
end
%=============================================================================
function bounds = validateBounds(bounds, nfactors)
  if ~isnumeric(bounds) || ndims(bounds) ~= 2 || size(bounds, 1) ~= nfactors
    error('Nelson:statistics:boundsOneRowFactor', 'Bounds must have one row per factor.');
  end
  if size(bounds, 2) < 2
    error('Nelson:statistics:boundsContainLeastTwoLevels', 'Bounds must contain at least two levels per factor.');
  end
end
%=============================================================================
function tf = isOptionName(value)
  tf = false;
  try
    optionName(value);
    tf = true;
  catch
    tf = false;
  end
end
%=============================================================================
function name = optionName(value)
  if ~isTextScalar(value)
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  text = lower(char(value));
  if strncmp('bounds', text, length(text))
    name = 'bounds';
  else
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
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
