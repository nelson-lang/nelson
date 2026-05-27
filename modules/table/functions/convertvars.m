%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function T = convertvars(T, vars, conversion)
  narginchk(3, 3);
  mustBeA(T, 'table', 1);
  names = T.Properties.VariableNames;
  idx = tableResolveVariables(names, vars);
  for k = idx
    name = names{k};
    if isa(conversion, 'function_handle')
      T.(name) = conversion(T.(name));
    elseif ischar(conversion) || isstring(conversion)
      T.(name) = cast(T.(name), char(conversion));
    else
      error(_('Conversion must be a function handle or type name.'));
    end
  end
end
%=============================================================================
