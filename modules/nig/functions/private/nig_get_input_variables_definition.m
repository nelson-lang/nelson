%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function txt = nig_get_input_variables_definition(NIG_FUNCTION)
  txt = {'    // INPUT VARIABLES';
  ''};
  n = 0;
  for k = NIG_FUNCTION.VARIABLES(:)'
    if strcmp(k.MODE, 'input')
      txt{end + 1} = ['    ArrayOf ', k.NAME, ' = argIn[', int2str(n), '];'];
      txt{end + 1} = ['    Dimensions dims', k.NAME, ' = ', k.NAME, '.getDimensions();'];
      
      if strcmp(k.TYPE, 'double_array')
        txt{end + 1} = ['    ', k.NAME, '.promoteType(', 'NLS_DOUBLE', ');'];
        txt{end + 1} = ['    double *',  k.NAME, '_ptr = (double*)', k.NAME, '.getDataPointer();'];
      end
      if strcmp(k.TYPE, 'integer_array')
        txt{end + 1} = ['    ', k.NAME, '.promoteType(', 'NLS_INT32', ');'];
        txt{end + 1} = ['    int *',  k.NAME, '_ptr = (int*)', k.NAME, '.getDataPointer();'];
      end
      if strcmp(k.TYPE, 'character')
        txt{end + 1} = ['    std::string ',  k.NAME, '_string', ' = ', k.NAME, '.getContentAsCString();'];
        txt{end + 1} = ['    const char* ',  k.NAME, '_ptr', ' = ', k.NAME, '_string.c_str();'];
      end
      if strcmp(k.TYPE, 'double')
        txt{end + 1} = ['    ', k.NAME, '.promoteType(', 'NLS_DOUBLE', ');'];
        txt{end + 1} = ['    double *',  k.NAME, '_ptr = (double*)', k.NAME, '.getDataPointer();'];
      end
      if strcmp(k.TYPE, 'integer')
        txt{end + 1} = ['    ', k.NAME, '.promoteType(', 'NLS_INT32', ');'];
        txt{end + 1} = ['    int *',  k.NAME, '_ptr = (int*)', k.NAME, '.getDataPointer();'];
      end
      n = n + 1;
    end
    if strcmp(k.MODE, 'in_out')
      n = n + 1;
    end
  end
  nArgout = n;
end
%=============================================================================
