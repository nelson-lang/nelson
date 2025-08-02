%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function txt = nig_check_dimensions_input_variables_definition(NIG_FUNCTION)
  txt = {'    // CHECK INPUT VARIABLES DIMENSIONS';
  ''};
  n = 0;
  for k = NIG_FUNCTION.VARIABLES(:)'
    NO_DIMENSIONS_CHECK = false;
    if isfield(k, 'NO_DIMENSIONS_CHECK')
      if isempty(k.NO_DIMENSIONS_CHECK)
        NO_DIMENSIONS_CHECK = false;
      else
        if k.NO_DIMENSIONS_CHECK == true
          NO_DIMENSIONS_CHECK = true;
        end
      end
    end
    if strcmp(k.MODE, 'in_out') || strcmp(k.MODE, 'input')
      if strcmp(k.TYPE, 'double') || strcmp(k.TYPE, 'integer') || strcmp(k.TYPE, 'character')
        if NO_DIMENSIONS_CHECK == false
          txt{end + 1} = ['    ', 'if (!dims', k.NAME, '.isScalar())'];
          txt{end + 1} = ['    {'];
          txt{end + 1} = ['        Error(_W("Input argument #', int2str(n + 1),': scalar expected."));'];
          txt{end + 1} = ['    }'];
        else
          txt{end + 1} = ['    ', '% DIMENSIONS NOT CHECKED FOR #', int2str(n + 1), ' ', k.NAME];
        end
      end
      if strcmp(k.TYPE, 'double_array') || strcmp(k.TYPE, 'integer_array')
        if ~isempty(k.DIMENSION_M) && ~isempty(k.DIMENSION_N)
          if NO_DIMENSIONS_CHECK == false
            txt{end + 1} = ['    Dimensions dims', k.NAME, '_expected(', nig_get_dimension_m_as_string(k), ', ', nig_get_dimension_n_as_string(k), ');'];
            txt{end + 1} = ['    ', 'if (!dims', k.NAME, '.equals(dims', k.NAME, '_expected))'];
            txt{end + 1} = ['    {'];
            txt{end + 1} = ['        Error(_("Input argument #', int2str(n + 1), ': wrong size.") + " " + dims', k.NAME,'_expected.toString() + " " + "expected" + ".");'];
            txt{end + 1} = ['    }'];
          else
            txt{end + 1} = ['    ', '% DIMENSIONS NOT CHECKED FOR #', int2str(n + 1), ' ', k.NAME];
          end
        end
      end
      n = n + 1;
    end
  end
end
%=============================================================================
