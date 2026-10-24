%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function T = clusterdata(varargin)
  narginchk(3, Inf);
  nargoutchk(0, 1);

  X = varargin{1};
  if ~isnumeric(X) || ndims(X) ~= 2 || size(X, 1) < 2
    error('Nelson:statistics:xNumericMatrixLeastTwo', 'X must be a numeric matrix with at least two rows.');
  end
  if rem(nargin - 1, 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  opts = parseOptions(varargin(2:end));
  if isempty(opts.Cutoff) == isempty(opts.MaxClust)
    error('Nelson:statistics:cutoffOrMaxClust', 'Specify either Cutoff or MaxClust.');
  end

  if isempty(opts.DistanceParameter)
    D = pdist(X, opts.Distance);
  else
    D = pdist(X, opts.Distance, opts.DistanceParameter);
  end
  Z = linkage(D, opts.Linkage);
  if isempty(opts.Cutoff)
    T = cluster(Z, 'MaxClust', opts.MaxClust, 'Criterion', opts.Criterion);
  else
    T = cluster(Z, 'Cutoff', opts.Cutoff, 'Criterion', opts.Criterion);
  end
end
%=============================================================================
function opts = parseOptions(args)
  opts = struct('Cutoff', [], 'MaxClust', [], 'Criterion', 'distance', ...
                'Distance', 'euclidean', 'DistanceParameter', [], ...
                'Linkage', 'single', 'SaveMemory', 'off', 'Depth', []);
  for k = 1:2:length(args)
    name = optionName(args{k});
    value = args{k + 1};
    switch name
      case 'cutoff'
        opts.Cutoff = numericVector(value, sprintf(_('%s must be numeric.'), 'Cutoff'));
      case 'maxclust'
        opts.MaxClust = numericVector(value, sprintf(_('%s must be numeric.'), 'MaxClust'));
      case 'criterion'
        opts.Criterion = criterionValue(value);
      case 'distance'
        opts.Distance = value;
      case 'linkage'
        opts.Linkage = value;
      case 'p'
        opts.DistanceParameter = positiveScalar(value, sprintf(_('%s must be a positive scalar.'), 'P'));
      case 'savememory'
        opts.SaveMemory = onOffValue(value);
      case 'depth'
        opts.Depth = positiveScalar(value, sprintf(_('%s must be a positive scalar.'), 'Depth'));
      otherwise
        error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
    end
  end
end
%=============================================================================
function name = optionName(value)
  if ~(ischar(value) || isStringScalar(value))
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Name');
  end
  text = lower(char(value));
  if isempty(text)
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  names = {'cutoff', 'maxclust', 'criterion', 'distance', 'linkage', 'p', 'savememory', 'depth'};
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
function value = criterionValue(value)
  if ~(ischar(value) || isStringScalar(value))
    error('Nelson:statistics:criterionDistance', 'Criterion must be ''distance''.');
  end
  text = lower(char(value));
  if ~isempty(text) && strncmp('distance', text, length(text))
    value = 'distance';
  else
    error('Nelson:statistics:criterionDistance', 'Criterion must be ''distance''.');
  end
end
%=============================================================================
function value = onOffValue(value)
  if ~(ischar(value) || isStringScalar(value))
    error('Nelson:statistics:saveMemoryOff', 'SaveMemory must be ''on'' or ''off''.');
  end
  text = lower(char(value));
  if ~isempty(text) && strncmp('on', text, length(text))
    value = 'on';
  elseif ~isempty(text) && strncmp('off', text, length(text))
    value = 'off';
  else
    error('Nelson:statistics:saveMemoryOff', 'SaveMemory must be ''on'' or ''off''.');
  end
end
%=============================================================================
function values = numericVector(value, message)
  if ~isnumeric(value) || ~isvector(value) || isempty(value)
    error('Nelson:statistics:mustBeNumericVector', message);
  end
  values = value(:)';
end
%=============================================================================
function value = positiveScalar(value, message)
  if ~isnumeric(value) || ~isscalar(value) || ~(value > 0)
    error('Nelson:statistics:mustBePositive', message);
  end
end
%=============================================================================
