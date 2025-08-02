%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function txt = nig_call_function(NIG_FUNCTION)
  txt = {'    // CALL EXTERN FUNCTION';
  ''};
  txt{end + 1} = '    try';
  txt{end + 1} = '    {';
  
  t = '';
  if strcmp(NIG_FUNCTION.LANGUAGE, 'fortran') == true
    t = ['        ', NIG_FUNCTION.SYMBOL, '_'];
  else
    t = ['        ', NIG_FUNCTION.SYMBOL];
  end
  t = [t, ' ('];
  for k = NIG_FUNCTION.VARIABLES(:)'
    if strcmp(k.MODE, 'input')
      t = [t, ' ', k.NAME, '_ptr'];
    end
    if strcmp(k.MODE, 'output')
      t = [t, ' ', k.NAME, '_output_ptr'];
    end
    if strcmp(k.MODE, 'in_out')
      t = [t, ' ', k.NAME, '_output_ptr'];
    end
    if strcmp(k.MODE, 'local')
      t = [t, ' ', k.NAME, '_ptr'];
    end
    
    t = [t, ','];
  end
  if t(end) == ','
    t= t(1:end - 1);
  end
  t = [t, ');'];
  txt{end + 1} = t;
  
  txt{end + 1} = '    }';
  txt{end + 1} = '    catch (const std::runtime_error &e)';
  txt{end + 1} = '    {';
  txt{end + 1} = '        e.what();';
  txt{end + 1} = ['        Error(', '"', NIG_FUNCTION.SYMBOL, ' function fails.");'];
  txt{end + 1} = '    }';
  
end
%=============================================================================
