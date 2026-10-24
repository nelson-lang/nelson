%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function checkDimensionVariableNames(obj, props)
  % A dimension name cannot also be a variable name.
  for name = props.DimensionNames
    if any(strcmp(props.VariableNames, name{1}))
      if istimetable(obj)
        id = 'Nelson:timetable:duplicateDimensionVariableNames';
      else
        id = 'Nelson:table:duplicateDimensionVariableNames';
      end
      error(id, _('Duplicate dimension and variable name: ''%s''.'), name{1});
    end
  end
end
%=============================================================================
