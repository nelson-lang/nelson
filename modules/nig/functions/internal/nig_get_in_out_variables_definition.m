%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
