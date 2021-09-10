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
function MException_display(e, name)
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
