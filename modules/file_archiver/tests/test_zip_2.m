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
builderFile = [nelsonroot(),'/module_skeleton/builder.m'];
if ~isfile(builderFile)
  return
end
%=============================================================================
TMPDIR = tempdir();
if ismac()
  TMPDIR = ['/private', TMPDIR];
end
%=============================================================================
DEST_2 = [TMPDIR, 'zip_test_2.zip'];
cd([nelsonroot(), '/module_skeleton'])
R = zip(DEST_2, [nelsonroot(), '/module_skeleton/*.m']);
REF1 = {'builder.m'};
REF2 = {'builder.m', 'loader.m'};
if length(R) == 2
  assert_isequal(R, REF2);
else
  assert_isequal(R, REF1);
end
assert_istrue(isfile(DEST_2));
%=============================================================================
temp_dest = [TMPDIR, createGUID()];
mkdir(temp_dest);
cd(temp_dest);
R2 = unzip(DEST_2);
REF1 = {[temp_dest, '/builder.m']};
REF2 = {[temp_dest, '/builder.m'] , [temp_dest, '/loader.m']};
if length(R2) == 2
  assert_isequal(R2, REF2);
else
  assert_isequal(R2, REF1);
end
cd(tempdir());
rmdir(temp_dest, 's');
%=============================================================================
