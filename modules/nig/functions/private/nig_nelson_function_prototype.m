%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function txt = nig_nelson_function_prototype(NIG_FUNCTION)
  txt = '';
  input = '';
  output = '';
  for k = NIG_FUNCTION.VARIABLES(:)'
    if strcmp(k.MODE, 'input') || strcmp(k.MODE, 'in_out')
      if isempty(input)
        if strcmp(k.MODE, 'in_out')
          input = [k.NAME, '_IN, '];
        else
          input = [k.NAME, ', '];
        end
      else
        if strcmp(k.MODE, 'in_out')
          input = [input, k.NAME, '_IN, '];
        else
          input = [input, k.NAME, ', '];
        end
      end
    end
    if strcmp(k.MODE, 'output') || strcmp(k.MODE, 'in_out')
      if isempty(output)
        if strcmp(k.MODE, 'in_out')
          output = [k.NAME, '_OUT, '];
        else
          output = [k.NAME, ', '];
        end
      else
        if strcmp(k.MODE, 'in_out')
          output = [output, k.NAME, '_OUT, '];
        else
          output = [output, k.NAME, ', '];
        end
      end
    end
  end
  if length(input) > 2
    if strcmp(input(end - 1:end), ', ') == true
      input = input(1:end - 2);
    end
  end
  if length(output) > 2
    if strcmp(output(end - 1:end), ', ') == true
      output = output(1:end - 2);
    end
  end
  
  txt = ['[', output, '] = ', NIG_FUNCTION.NELSON_NAME, '(', input, ')'];
end
%=============================================================================
