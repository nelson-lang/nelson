%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function mdl = fitensemble(X, Y, method, numLearningCycles, learners, varargin)
  narginchk(5, Inf);
  nargoutchk(0, 1);
  if rem(length(varargin), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  [problemType, args] = extractTypeLocal(varargin, Y);
  commonArgs = [{'Method', method, 'NumLearningCycles', numLearningCycles, ...
    'Learners', learners}, args];
  if strcmp(problemType, 'regression')
    mdl = fitrensemble(X, Y, commonArgs{:});
  else
    mdl = fitcensemble(X, Y, commonArgs{:});
  end
end
%=============================================================================
function [problemType, args] = extractTypeLocal(args, Y)
  problemType = defaultTypeLocal(Y);
  keep = true(1, length(args));
  for k = 1:2:length(args)
    if isTypeNameLocal(args{k})
      problemType = typeLocal(args{k + 1});
      keep(k) = false;
      keep(k + 1) = false;
    end
  end
  args = args(keep);
end
%=============================================================================
function tf = isTypeNameLocal(value)
  tf = false;
  if ~(ischar(value) || (isstring(value) && isscalar(value)))
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  text = lower(char(value));
  tf = startsWith('type', text);
end
%=============================================================================
function problemType = defaultTypeLocal(Y)
  if isnumeric(Y) && isreal(Y)
    problemType = 'regression';
  else
    problemType = 'classification';
  end
end
%=============================================================================
function problemType = typeLocal(value)
  if ~(ischar(value) || (isstring(value) && isscalar(value)))
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Type');
  end
  text = lower(char(value));
  choices = {'classification', 'regression'};
  matches = choices(startsWith(choices, text));
  if length(matches) ~= 1
    error('Nelson:statistics:invalidTypeValue', 'Invalid %s value.', 'Type');
  end
  problemType = matches{1};
end
%=============================================================================
