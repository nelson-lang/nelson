%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
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
assert_istrue(u3.MemUsedNelson - u2.MemUsedNelson <= 340000)
disp(u2.MemUsedNelson - u1.MemUsedNelson)
assert_istrue(u2.MemUsedNelson - u1.MemUsedNelson < 15000000)
disp(u3.MemUsedNelson - u1.MemUsedNelson)
assert_istrue(u3.MemUsedNelson - u1.MemUsedNelson < 15000000)
