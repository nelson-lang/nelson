%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = sameVariableProperties(obj, a, b)
  % True when the Properties structs a and b hold the same per-variable
  % metadata.
  names = {'VariableNames', 'VariableUnits', 'VariableDescriptions', 'VariableContinuity', 'CustomProperties'};
  tf = isstruct(b) && isstruct(a);
  for k = 1:length(names)
    if ~tf
      return
    end
    tf = isfield(a, names{k}) && isfield(b, names{k}) && isequal(a.(names{k}), b.(names{k}));
  end
end
%=============================================================================
