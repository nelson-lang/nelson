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
% <--ENGLISH IMPOSED-->
%=============================================================================
if ~isbuiltin('mxAssertS')
   test_dir = [tempdir(), 'mxAssertS'];
   if isdir(test_dir)
      rmdir(test_dir,'s');
   end
   mkdir(test_dir);
   status = copyfile('mxAssertS.c', test_dir);
   assert_istrue(status);
   cd(test_dir);
   mex('mxAssertS.c');
   run('loader.m');
end
%=============================================================================
R = mxAssertS(1);
%=============================================================================
M = [];
try
   mxAssertS(0);
catch
   M = lasterror();
end
assert_isequal(M.identifier, 'Nelson:MEX');
assert_istrue(contains(M.message, 'Assertion failed: at'));
%=============================================================================