%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function st = tableDeleteVariables(st, vars)
  % Removes the variables vars (indices) with their per-variable metadata.
  % Metadata left unset (not one entry per variable) is kept as it is.
  vars = unique(vars);
  names = st.Properties.VariableNames;
  st.Properties.CustomProperties = table.variableCustomPropertiesSubset(st, setdiff(1:length(names), vars));
  % Remove every field in one rmfield call: a per-variable loop rebuilds the
  % whole data struct each time, making a multi-variable removal O(nVars^2).
  if ~isempty(vars)
    st.data = rmfield(st.data, names(vars));
  end
  nVars = length(names);
  perVariable = {'VariableNames', 'VariableTypes', 'VariableDescriptions', 'VariableUnits', 'VariableContinuity'};
  for k = 1:length(perVariable)
    field = perVariable{k};
    if isfield(st.Properties, field) && numel(st.Properties.(field)) == nVars
      st.Properties.(field)(vars) = [];
    end
  end
end
%=============================================================================
