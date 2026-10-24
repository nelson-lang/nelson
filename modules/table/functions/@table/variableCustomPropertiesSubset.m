%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function custom = variableCustomPropertiesSubset(st, order)
  % The custom properties of the table struct st, the per-variable ones set
  % (one element per variable) indexed by the variable indices order.
  custom = st.Properties.CustomProperties;
  if ~isfield(st, 'VariableCustomProperties') || ~isstruct(custom)
    return
  end
  nVars = length(st.Properties.VariableNames);
  names = st.VariableCustomProperties;
  for k = 1:length(names)
    if isfield(custom, names{k}) && numel(custom.(names{k})) == nVars
      custom.(names{k}) = custom.(names{k})(order);
    end
  end
end
%=============================================================================
