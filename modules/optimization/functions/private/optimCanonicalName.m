%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function name = optimCanonicalName(inputName)
  if isstring(inputName)
    inputName = char(inputName);
  end
  if ~ischar(inputName) || size(inputName, 1) ~= 1
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Option name');
  end
  direct = directCanonicalName(inputName);
  if ~isempty(direct)
    name = direct;
    return
  end
  names = optimSupportedOptions();
  matches = false(size(names));
  lowerInput = lower(inputName);
  for k = 1:length(names)
    name = lower(names{k});
    if length(name) >= length(lowerInput)
      matches(k) = strcmp(name(1:length(lowerInput)), lowerInput);
    end
  end
  if sum(matches) == 0
    error('Nelson:optimization:unknownOption', 'Unrecognized optimization option "%s".', inputName);
  end
  if sum(matches) > 1
    exact = false(size(names));
    for k = 1:length(names)
      exact(k) = strcmp(lower(names{k}), lowerInput);
    end
    if sum(exact) == 1
      matches = exact;
    else
      error('Nelson:optimization:ambiguousOption', 'Ambiguous optimization option "%s".', inputName);
    end
  end
  idx = find(matches, 1);
  name = names{idx};
end
%=============================================================================
function name = directCanonicalName(inputName)
  switch lower(inputName)
    case 'enablefeasibilitymode'
      name = 'EnableFeasibilityMode';
    case 'hessianapproximation'
      name = 'HessianApproximation';
    case 'usecodegensolver'
      name = 'UseCodegenSolver';
    case 'barrierparamupdate'
      name = 'BarrierParamUpdate';
    case 'maxpcgiter'
      name = 'MaxPCGIter';
    case 'maxprojcgiter'
      name = 'MaxProjCGIter';
    case 'tolconsqp'
      name = 'TolConSQP';
    case 'tolprojcg'
      name = 'TolProjCG';
    case 'tolprojcgabs'
      name = 'TolProjCGAbs';
    case 'honorbounds'
      name = 'HonorBounds';
    otherwise
      name = '';
  end
end
%=============================================================================
