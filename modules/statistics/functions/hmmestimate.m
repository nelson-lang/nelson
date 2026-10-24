%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [trans, emis] = hmmestimate(seq, states, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 2);
  opts = parseOptions(varargin);
  rawVector(seq, 'SEQ');
  rawVector(states, 'STATES');
  nstates = max(max(double(states(:))), opts.NStates);
  nsymbols = max(max(double(seq(:))), opts.NSymbols);
  seq = __hmm_check_sequence(seq, nsymbols, 'SEQ');
  states = __hmm_check_sequence(states, nstates, 'STATES');
  if length(seq) ~= length(states)
    error('Nelson:statistics:SEQSTATESSameLength', '%s must have the same length.', 'SEQ and STATES');
  end
  opts.PseudoTransitions = expandPseudo(opts.PseudoTransitions, [nstates nstates], ...
    sprintf(_('%s has incompatible size.'), 'PseudoTransitions'));
  opts.PseudoEmissions = expandPseudo(opts.PseudoEmissions, [nstates nsymbols], ...
    sprintf(_('%s has incompatible size.'), 'PseudoEmissions'));
  trans = zeros(nstates, nstates);
  emis = zeros(nstates, nsymbols);
  for t = 1:length(seq)
    emis(states(t), seq(t)) = emis(states(t), seq(t)) + 1;
    if t < length(seq)
      trans(states(t), states(t + 1)) = trans(states(t), states(t + 1)) + 1;
    end
  end
  trans = normalizeCounts(trans + opts.PseudoTransitions);
  emis = normalizeCounts(emis + opts.PseudoEmissions);
end
%=============================================================================
function rawVector(value, name)
  if ~(isnumeric(value) && isreal(value) && isvector(value) && ~isempty(value))
    error('Nelson:statistics:mustBeNonemptyVector', '%s must be a nonempty numeric vector.', name);
  end
end
%=============================================================================
function opts = parseOptions(args)
  opts = struct('NStates', 0, 'NSymbols', 0, 'PseudoTransitions', 0, 'PseudoEmissions', 0);
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(args)
    name = optionName(args{k});
    value = args{k + 1};
    switch name
      case 'nstates'
        opts.NStates = positiveInteger(value, sprintf(_('%s must be a positive integer scalar.'), 'NStates'));
      case 'nsymbols'
        opts.NSymbols = positiveInteger(value, sprintf(_('%s must be a positive integer scalar.'), 'NSymbols'));
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
  names = {'nstates', 'nsymbols', 'pseudotransitions', 'pseudoemissions'};
  text = lower(char(value));
  matches = names(startsWith(names, text));
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
