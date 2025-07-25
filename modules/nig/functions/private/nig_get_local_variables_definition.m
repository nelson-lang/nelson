%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function txt = nig_get_local_variables_definition(NIG_FUNCTION)
  txt = {'    // LOCAL VARIABLES';
  ''};
  for k = NIG_FUNCTION.VARIABLES(:)'
    if strcmp(k.MODE, 'local')
      if strcmp(k.TYPE, 'double_array')
        if isfield(k, 'DIMENSION_M') && ~isempty(k.DIMENSION_M)
          if isnumeric(k.DIMENSION_M)
            DIMENSION_M_str = int2str(k.DIMENSION_M);
          else
            DIMENSION_M_str = k.DIMENSION_M;
          end
        else
          DIMENSION_M_str = '1';
        end
        if isfield(k, 'DIMENSION_N') && ~isempty(k.DIMENSION_N)
          if isnumeric(k.DIMENSION_N)
            DIMENSION_N_str = int2str(k.DIMENSION_N);
          else
            DIMENSION_N_str = k.DIMENSION_N;
          end
        else
          DIMENSION_N_str = '1';
        end
        
        txt{end + 1} = ['    ArrayOf ', k.NAME, ' = ArrayOf::doubleMatrix2dConstructor(', DIMENSION_M_str, ' , ', DIMENSION_N_str ,');'];
        txt{end + 1} = ['    double * ', k.NAME, '_ptr = (double*)', k.NAME, '.getDataPointer();'];
        
      end
      if strcmp(k.TYPE, 'integer_array')
        if isfield(k, 'DIMENSION_M') && ~isempty(k.DIMENSION_M)
          if isnumeric(k.DIMENSION_M)
            DIMENSION_M_str = int2str(k.DIMENSION_M);
          else
            DIMENSION_M_str = k.DIMENSION_M;
          end
        else
          DIMENSION_M_str = '1';
        end
        if isfield(k, 'DIMENSION_N') && ~isempty(k.DIMENSION_N)
          if isnumeric(k.DIMENSION_N)
            DIMENSION_N_str = int2str(k.DIMENSION_N);
          else
            DIMENSION_N_str = k.DIMENSION_N;
          end
        else
          DIMENSION_N_str = '1';
        end
        txt{end + 1} = ['    ArrayOf ', k.NAME, ' = ArrayOf::int32Matrix2dConstructor(', DIMENSION_M_str, ' , ', DIMENSION_N_str ,');'];
        txt{end + 1} = ['    int* ', k.NAME, '_ptr = (int*)', k.NAME, '.getDataPointer();'];
        
      end
      if strcmp(k.TYPE, 'double')
        if isfield(k, 'VALUE') && ~isempty(k.VALUE)
          txt{end + 1} = ['    ArrayOf ', k.NAME, ' = ArrayOf::doubleRowVectorConstructor(1);'];
          txt{end + 1} = ['    double*',  k.NAME, '_ptr', ' = (double*)', k.NAME, '.getDataPointer();'];
          txt{end + 1} = ['    ', k.NAME, '_ptr[0] = ', k.VALUE, ';'];
        else
          txt{end + 1} = ['    ArrayOf ', k.NAME, ' = ArrayOf::doubleRowVectorConstructor(1);'];
          txt{end + 1} = ['    double*',  k.NAME, '_ptr', ' = (double*)', k.NAME, '.getDataPointer();'];
        end
      end
      if strcmp(k.TYPE, 'integer')
        if isfield(k, 'VALUE') && ~isempty(k.VALUE)
          txt{end + 1} = ['    ArrayOf ', k.NAME, ' = ArrayOf::int32RowVectorConstructor(1);'];
          txt{end + 1} = ['    int* ', k.NAME, '_ptr', ' = (int*)', k.NAME, '.getDataPointer();'];
          txt{end + 1} = ['    ', k.NAME, '_ptr[0] = ', k.VALUE, ';'];
        else
          txt{end + 1} = ['    ArrayOf ', k.NAME, ' = ArrayOf::int32RowVectorConstructor(1);'];
          txt{end + 1} = ['    int* ', k.NAME, '_ptr', ' = (int*)', k.NAME, '.getDataPointer();'];
        end
      end
    end
  end
  
end
%=============================================================================
