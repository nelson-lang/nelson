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
assert_isequal(nargin('zip'), -3);
assert_isequal(nargout('zip'), -1);
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
DEST_1 = [TMPDIR, 'zip_test_1.zip'];
cd([nelsonroot(), '/module_skeleton'])
R = zip(DEST_1, '*.m');
REF1 = {'builder.m'};
REF2 = {'builder.m', 'loader.m'};
if length(R) == 2
  assert_isequal(R, REF2);
else
  assert_isequal(R, REF1);
end
assert_istrue(isfile(DEST_1));
%=============================================================================
temp_dest = [TMPDIR, createGUID()];
mkdir(temp_dest);
cd(temp_dest);
R1 = unzip(DEST_1);
REF1 = {[temp_dest, '/builder.m']};
REF2 = {[temp_dest, '/builder.m'] , [temp_dest, '/loader.m']};
if length(R1) == 2
  assert_isequal(R1, REF2);
else
  assert_isequal(R1, REF1);
end
cd(tempdir());
rmdir(temp_dest, 's');
%=============================================================================
cmd = 'R = zip(DEST_1, ''*.m'', [nelsonroot(), ''/modules_skeleton''])';
assert_checkerror(cmd, _('Invalid root path.'));
%=============================================================================
