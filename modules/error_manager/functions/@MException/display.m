%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function display(e, name)
  fmt = format();
  if ~isempty(name)
    if strcmp(fmt.LineSpacing, 'loose') == true
      fprintf(char(10))  
    end
    fprintf([name, ' = ', char(10)])
    if strcmp(fmt.LineSpacing, 'loose') == true
      fprintf(char(10))  
    end
  end
  fprintf('  %s with properties:', class(e))
  fprintf(char(10))
  r = struct(e);
  d = evalc('disp(r);');
  fprintf(d);
  if strcmp(fmt.LineSpacing, 'loose') == true
    fprintf(char(10))  
  end
end
%=============================================================================
