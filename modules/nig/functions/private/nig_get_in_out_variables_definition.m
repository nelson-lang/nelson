%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function txt = nig_get_in_out_variables_definition(NIG_FUNCTION)
  txt = {'    // IN/OUT VARIABLES';
  ''};
  n = 0;
  for k = NIG_FUNCTION.VARIABLES(:)'
    if strcmp(k.MODE, 'input')
      n = n + 1;
    end
    if strcmp(k.MODE, 'in_out')
      txt{end + 1} = ['    ArrayOf ', k.NAME, ' = argIn[', int2str(n), '];'];
      txt{end + 1} = ['    Dimensions dims', k.NAME, ' = ', k.NAME, '.getDimensions();'];
      
      if strcmp(k.TYPE, 'double_array') || strcmp(k.TYPE, 'double')
        txt{end + 1} = ['    ', k.NAME, '.promoteType(', 'NLS_DOUBLE', ');'];
      end
      
      if strcmp(k.TYPE, 'integer_array') || strcmp(k.TYPE, 'integer')
        txt{end + 1} = ['    ', k.NAME, '.promoteType(', 'NLS_INT32', ');'];
      end
      
      txt{end + 1} = ['    ArrayOf ', k.NAME, '_output = ', k.NAME, ';'];
      txt{end + 1} = ['    ', k.NAME, '_output.ensureSingleOwner();'];
      
      if strcmp(k.TYPE, 'character')
        % ???
        txt{end + 1} = ['    std::string ',  k.NAME, '_input_string', ' = ', k.NAME, '.getContentAsCString();'];
        txt{end + 1} = ['    const char* ',  k.NAME, '_ptr', ' = ', k.NAME, '_input_string.c_str();'];
      end
      
      if strcmp(k.TYPE, 'double_array') || strcmp(k.TYPE, 'double')
        txt{end + 1} = ['    double *', k.NAME, '_output_ptr = (double*)', k.NAME, '_output.getDataPointer();'];
        
      end
      
      if strcmp(k.TYPE, 'integer_array') || strcmp(k.TYPE, 'integer')
        txt{end + 1} = ['    int *', k.NAME, '_output_ptr = (int*)', k.NAME, '_output.getDataPointer();'];
      end
      
      n = n + 1;
    end
  end
end
%=============================================================================
