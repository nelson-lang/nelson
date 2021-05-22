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
% <--ADV-CLI MODE-->
% <--WITH DISPLAY-->
%=============================================================================
assert_isequal(nargin('invoke'), 2);
assert_isequal(nargout('invoke'), 1);
%=============================================================================
qml_file_ok = [modulepath('qml_engine'), '/tests/test_qml_methods.qml'];
qobj = qml_loadfile(qml_file_ok);
%=============================================================================
if ismethod(qobj.children(2), 'myQmlFunction1')
  idx = 2;
else
  idx = 3;
end
%=============================================================================
R = invoke(qobj.children(idx), 'myQmlFunction1', 1);
REF = {1, int32(1),  int32(2),  int32(3)};
assert_isequal(R, REF);
%=============================================================================
R = invoke(qobj.children(idx), 'myQmlFunction1', 'hello');
REF = {'hello', int32(1),  int32(2),  int32(3)};
assert_isequal(R, REF);
%=============================================================================
R = invoke(qobj.children(idx), 'myQmlFunction3', 'hello');
REF = [];
assert_isequal(R, REF);
%=============================================================================
delete(qobj);
%=============================================================================
