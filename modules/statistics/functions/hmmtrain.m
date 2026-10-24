%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [trans, emis] = hmmtrain(seq, trans, emis, varargin)
  narginchk(3, Inf);
  nargoutchk(0, 2);
  [trans, emis, ~, nsymbols] = __hmm_check_model(trans, emis);
  seq = __hmm_check_sequence(seq, nsymbols, 'SEQ');
  opts = parseOptions(varargin);
  opts.PseudoTransitions = expandPseudo(opts.PseudoTransitions, size(trans), ...
    sprintf(_('%s has incompatible size.'), 'PseudoTransitions'));
  opts.PseudoEmissions = expandPseudo(opts.PseudoEmissions, size(emis), ...
    sprintf(_('%s has incompatible size.'), 'PseudoEmissions'));
  previous = NaN;
  for iter = 1:opts.MaxIterations
    [gamma, xiSum, logp] = __hmm_forward_backward(seq, trans, emis);
    trans = normalizeCounts(xiSum + opts.PseudoTransitions);
    emisCounts = zeros(size(emis));
    for t = 1:length(seq)
      emisCounts(:, seq(t)) = emisCounts(:, seq(t)) + gamma(t, :)';
    end
    emis = normalizeCounts(emisCounts + opts.PseudoEmissions);
    if opts.Verbose
      disp(['iteration ', int2str(iter), ', log likelihood = ', num2str(logp)]);
    end
    if isfinite(previous) && abs(logp - previous) <= opts.Tolerance * max(1, abs(previous))
      break
    end
    previous = logp;
  end
end
%=============================================================================
function opts = parseOptions(args)
  opts = struct('MaxIterations', 100, 'Tolerance', 1e-6, ...
    'Verbose', false, 'PseudoTransitions', 0, 'PseudoEmissions', 0);
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(args)
    name = optionName(args{k});
    value = args{k + 1};
    switch name
      case 'maxiterations'
        opts.MaxIterations = positiveInteger(value, sprintf(_('%s must be a positive integer scalar.'), 'MaxIterations'));
      case 'tolerance'
        opts.Tolerance = positiveScalar(value, sprintf(_('%s must be a positive scalar.'), 'Tolerance'));
      case 'verbose'
        opts.Verbose = logicalScalar(value, sprintf(_('%s must be a logical scalar.'), 'Verbose'));
      case 'pseudotransitions'
        opts.PseudoTransitions = nonnegativeMatrix(value, sprintf(_('%s must be nonnegative.'), 'PseudoTransitions'));
      case 'pseudoemissions'
        opts.PseudoEmissions = nonnegativeMatrix(value, sprintf(_('%s must be nonnegative.'), 'PseudoEmissions'));
    end
  end
end
%=============================================================================
function name = optionName(value)
  if ~(ischar(value) || isStringScalar(value))
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Name');
  end
  names = {'maxiterations', 'tolerance', 'verbose', 'pseudotransitions', 'pseudoemissions'};
  matches = names(startsWith(names, lower(char(value))));
  if length(matches) ~= 1
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  name = matches{1};
end
%=============================================================================
function value = positiveInteger(value, message)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) ...
      && floor(value) == value && value > 0)
    error('Nelson:statistics:mustBePositiveInteger', message);
  end
  value = double(value);
end
%=============================================================================
function value = positiveScalar(value, message)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) && value > 0)
    error('Nelson:statistics:mustBePositive', message);
  end
  value = double(value);
end
%=============================================================================
function value = logicalScalar(value, message)
  if ~((islogical(value) || isnumeric(value)) && isscalar(value) && (value == 0 || value == 1))
    error('Nelson:validators:mustBeLogicalScalar', message);
  end
  value = logical(value);
end
%=============================================================================
function value = nonnegativeMatrix(value, message)
  if ~(isnumeric(value) && isreal(value) && ~issparse(value) && ndims(value) == 2) ...
      || any(~isfinite(double(value(:)))) || any(double(value(:)) < 0)
    error('Nelson:statistics:mustBeNonnegative', message);
  end
  value = double(value);
end
%=============================================================================
function value = expandPseudo(value, targetSize, message)
  if isscalar(value)
    value = repmat(value, targetSize);
  elseif ~isequal(size(value), targetSize)
    error('Nelson:statistics:sizeMismatch', message);
  end
end
%=============================================================================
function A = normalizeCounts(A)
  rowsum = sum(A, 2);
  for i = 1:size(A, 1)
    if rowsum(i) > 0
      A(i, :) = A(i, :) / rowsum(i);
    else
      A(i, :) = 1 / size(A, 2);
    end
  end
end
%=============================================================================
