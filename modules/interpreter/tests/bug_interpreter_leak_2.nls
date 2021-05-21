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
clear('functions');
[builtin_list, macro_list] = what();
[u1, s1] = memory();
filtered = { 'buildhelp';
 'buildhelpmd';
 'buildhelpweb';
 'configuremingw';
 'configuremsvc';
 'doc';
 'edit';
 'errordlg';
 'helpdlg';
 'indexhelp';
 'msgbox';
 'qml_demos';
 'questdlg';
 'removecompilerconf';
 'test_run';
 'vswhere';
 'warndlg';
 'qcollectiongenerator';
 'license'};


for k = 1:length(macro_list)
  if strcmp(macro_list{k}, filtered) == false
    execstr([macro_list{k}, ';'], 'errcatch');
  end
end
[u2, s2] = memory();
clear('functions');
[u3, s3] = memory();
disp(u3.MemUsedNelson - u2.MemUsedNelson)
assert_istrue(u3.MemUsedNelson - u2.MemUsedNelson <= 300000)
disp(u2.MemUsedNelson - u1.MemUsedNelson)
assert_istrue(u2.MemUsedNelson - u1.MemUsedNelson < 15000000)
disp(u3.MemUsedNelson - u1.MemUsedNelson)
assert_istrue(u3.MemUsedNelson - u1.MemUsedNelson < 15000000)
