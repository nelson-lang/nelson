%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function checkReservedVariableNames(obj, names)
  % Names reserved by the table interface cannot name a variable.
  for name = {'Properties', 'RowNames', 'VariableNames', ':'}
    if any(strcmp(names, name{1}))
      if istimetable(obj)
        id = 'Nelson:timetable:reservedVariableNameConflict';
      else
        id = 'Nelson:table:reservedVariableNameConflict';
      end
      error(id, _('Table variable name conflicts with reserved name ''%s''.'), name{1});
    end
  end
end
%=============================================================================
