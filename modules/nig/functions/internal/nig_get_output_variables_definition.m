%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function txt = nig_get_output_variables_definition(NIG_FUNCTION)
  txt = {'    // OUTPUT VARIABLES';
  ''};
  for k = NIG_FUNCTION.VARIABLES(:)'
    if strcmp(k.MODE, 'output')
      if strcmp(k.TYPE, 'double_array')
        txt{end + 1} = ['    ArrayOf ', k.NAME, '_output = ArrayOf::doubleMatrix2dConstructor((indexType)', k.DIMENSION_M, ', (indexType)', k.DIMENSION_N, ');'];
        txt{end + 1} = ['    double *',  k.NAME, '_output_ptr = (double*)', k.NAME, '_output.getDataPointer();'];
      end
      
      if strcmp(k.TYPE, 'integer_array')
        txt{end + 1} = ['    ArrayOf ', k.NAME, '_output = ArrayOf::int32Matrix2dConstructor((indexType)', k.DIMENSION_M, ', (indexType)', k.DIMENSION_N, ');'];
        txt{end + 1} = ['    int *',  k.NAME, '_output_ptr = (int*)', k.NAME, '_output.getDataPointer();'];
      end
      
      if strcmp(k.TYPE, 'double')
        txt{end + 1} = ['    ArrayOf ', k.NAME, '_output = ArrayOf::doubleVectorConstructor(1);'];
        txt{end + 1} = ['    double *',  k.NAME, '_output_ptr = (double*)', k.NAME, '_output.getDataPointer();'];
        
      end
      
      if strcmp(k.TYPE, 'integer')
        txt{end + 1} = ['    ArrayOf ', k.NAME, '_output = ArrayOf::int32VectorConstructor(1);'];
        txt{end + 1} = ['    int *',  k.NAME, '_output_ptr = (int*)', k.NAME, '_output.getDataPointer();'];
      end
    end
  end
  
end
%=============================================================================
