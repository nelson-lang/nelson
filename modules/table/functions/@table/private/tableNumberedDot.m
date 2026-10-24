%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function s = tableNumberedDot(variableNames, s, isAssignment)
  % T.(n): a number as dynamic field name is the variable number n. An
  % assignment may create the variable that follows the last one (Var<n>).
  subs = s(1).subs;
  if ~strcmp(s(1).type, '.') || ischar(subs) || isstring(subs)
    return
  end
  if ~isnumeric(subs)
    error('Nelson:table:illegalVarSubscript', _('A table variable name must be a string scalar or a character vector.'));
  end
  if ~isscalar(subs) || ~isreal(subs) || subs < 1 || subs ~= fix(subs)
    error('Nelson:table:illegalVarIndex', _('A variable index must be a real positive integer.'));
  end
  if subs <= length(variableNames)
    s(1).subs = variableNames{subs};
  elseif isAssignment
    names = tableNumberedNewVariables(variableNames, double(subs));
    s(1).subs = names{1};
  else
    error('Nelson:table:varIndexOutOfRange', _('Variable index exceeds table dimensions.'));
  end
end
%=============================================================================
