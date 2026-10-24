%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('ismatfile'), 1);
asserts.isequal(nargout('ismatfile'), 1);
%=============================================================================
mat_dir = [fileparts(nfilename('fullpathext'), 'path'), '/mat/'];
%=============================================================================
[tf, ver] = ismatfile({[mat_dir, 'test_cell_nest_7.4_GLNX86.mat']});
asserts.istrue(tf);
asserts.isequal(ver, "-v7");
%=============================================================================
[tf, ver] = ismatfile(string([mat_dir, 'test_cell_nest_7.4_GLNX86.mat']));
asserts.istrue(tf);
asserts.isequal(ver, "-v7");
%=============================================================================
[tf, ver] = ismatfile([mat_dir, 'test_cell_nest_7.4_GLNX86.mat']);
asserts.istrue(tf);
asserts.isequal(ver, "-v7");
%=============================================================================
[tf, ver] = ismatfile([mat_dir, 'test_not_exist.mat']);
asserts.isfalse(tf);
asserts.isequal(ver, string());
%=============================================================================
[tf, ver, header] = ismatfile([mat_dir, 'test_cell_nest_7.4_GLNX86.mat']);
asserts.istrue(tf);
asserts.isequal(ver, "-v7");
header_ref = "MATLAB 5.0 MAT-file, Platform: GLNX86, Created on: Fri Feb 20 15:26:59 2009";
asserts.isequal(header, header_ref);
%=============================================================================
[tf, ver, header] = ismatfile([mat_dir, 'test_not_exist.mat']);
asserts.isfalse(tf);
asserts.isequal(ver, string());
asserts.isequal(header, string());
%=============================================================================
