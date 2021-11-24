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
function r = hilb(n, classname)
  % https://nhigham.com/2020/06/30/what-is-the-hilbert-matrix/
  narginchk(1, 2);
  if nargin < 2
    classname = 'double';
  end
  if (isStringScalar(classname)) || (~strcmp(classname, 'double') && ~strcmp(classname,'single'))
    error('Nelson:hilb:notSupportedClass', _('#2 argument must be ''double'' or ''single''.'));
  end
  j = 1:cast(n(1), classname);
  r = 1 ./ (j' + j - 1);
end
%=============================================================================
