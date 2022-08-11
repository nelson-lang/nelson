%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function txt = nig_assign_output_variables(NIG_FUNCTION)
  txt = {'    // ASSIGN OUTPUT VARIABLES';
  ''};
  n = 0;
  for k = NIG_FUNCTION.VARIABLES(:)'
    if strcmp(k.MODE, 'output') || strcmp(k.MODE, 'in_out')
      txt{end + 1} = ['    if (nLhs > ', int2str(n), ')'];
      txt{end + 1} = '    {';
      txt{end + 1} = ['        retval << ', k.NAME, ';'];
      txt{end + 1} = '    }';
      n = n + 1;
    end
  end
  
end
%=============================================================================
