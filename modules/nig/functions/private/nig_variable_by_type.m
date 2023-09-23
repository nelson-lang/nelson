%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function txt = nig_variable_by_type(VARIABLE, withLocal)
  txt = '';
  
  if strcmp(VARIABLE.TYPE, 'character')
    txt = nig_variable_character(VARIABLE, withLocal);
  end
  
  if strcmp(VARIABLE.TYPE, 'double_array')
    txt = nig_variable_double_array(VARIABLE, withLocal);
  end
  
  if strcmp(VARIABLE.TYPE, 'integer_array')
    txt = nig_variable_integer_array(VARIABLE, withLocal);
  end
  
  if strcmp(VARIABLE.TYPE, 'double')
    txt = nig_variable_double(VARIABLE, withLocal);
  end
  
  if strcmp(VARIABLE.TYPE, 'integer')
    txt = nig_variable_integer(VARIABLE, withLocal);
  end
  
end
%=============================================================================
