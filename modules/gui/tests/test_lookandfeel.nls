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
%=============================================================================
assert_isequal(nargin('lookandfeel'), 2);
assert_isequal(nargout('lookandfeel'), 1);
%=============================================================================
currentlf = lookandfeel();
assert_istrue(ischar(currentlf));
lfs = lookandfeel('available');
assert_istrue(iscell(lfs));
for lf = lfs'
  c = lookandfeel(lf{1});
end
r = lookandfeel(currentlf);
assert_istrue(ischar(r));
%=============================================================================
currentstylesheet = lookandfeel('stylesheet');
assert_istrue(ischar(currentstylesheet));
stylefilename = [modulepath('gui'), '/resources/darkstyle.qss'];
newstyle = fileread(stylefilename);
previousstylesheet = lookandfeel('stylesheet', newstyle);
assert_isequal(currentstylesheet, previousstylesheet);
r = lookandfeel('stylesheet', previousstylesheet);
%=============================================================================