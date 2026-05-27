%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function names = tableMakeValidName(names)
  if ischar(names)
    names = {names};
  elseif isstring(names)
    names = cellstr(names);
  end
  names = names(:)';
  for k = 1:length(names)
    name = names{k};
    if isempty(name)
      name = ['Var', num2str(k)];
    end
    name = regexprep(name, '[^a-zA-Z0-9_]', '_');
    if isempty(regexp(name(1), '[a-zA-Z]', 'once'))
      name = ['Var', name];
    end
    names{k} = name;
  end
end
%=============================================================================
