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
% <--INTERACTIVE TEST-->
% <--GUI MODE-->
%=============================================================================
assert_isequal(nargin('questdlg'), -1);
assert_isequal(nargout('questdlg'), 1);
%=============================================================================
r = questdlg('Hello')
r = questdlg('Hello', 'Franck')
r = questdlg('What is the answer to the ultimate question of life, the universe and everything ?', 'A question for geeks', '41', '42', '43', '42')
r = questdlg ('Easy ?', 'Jeff', 'No', 'Okay', 'Okay')
r = questdlg({'Is', 'this', 'a', 'multi line', 'test ?'}, 'Test :)')
r = questdlg('How are you ?', 'Health', 'Fine', 'Good', 'sick', 'Fine')
%=============================================================================
