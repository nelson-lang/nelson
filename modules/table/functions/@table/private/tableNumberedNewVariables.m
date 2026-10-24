%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function names = tableNumberedNewVariables(variableNames, sub)
  % T{rows, n} = value or T(rows, n) = value with variable numbers past the
  % width: the numbers become names. The new variables must follow the last
  % one without a gap; each gets the default name Var<n>, made unique.
  width = length(variableNames);
  sub = sub(:)';
  newNumbers = unique(sub(sub > width));
  if ~isequal(newNumbers, width + 1:width + length(newNumbers))
    error('Nelson:table:discontiguousVars', _('Cannot create a table variable with a discontiguous index.'));
  end
  used = variableNames;
  newNames = cell(1, length(newNumbers));
  for k = 1:length(newNumbers)
    base = sprintf('Var%d', newNumbers(k));
    candidate = base;
    suffix = 1;
    while any(strcmp(used, candidate))
      candidate = sprintf('%s_%d', base, suffix);
      suffix = suffix + 1;
    end
    newNames{k} = candidate;
    used{end + 1} = candidate;
  end
  names = cell(1, length(sub));
  for k = 1:length(sub)
    if sub(k) <= width
      names{k} = variableNames{sub(k)};
    else
      names{k} = newNames{sub(k) - width};
    end
  end
end
%=============================================================================
