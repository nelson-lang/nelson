%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
destinationdir = tempname();
mkdir(destinationdir);
currentpath = fileparts(nfilename('fullpathext'));
sourcefile = [currentpath, '/test_build_c.c'];
[status, message, generatedfilename] = dlgeneratemake(destinationdir, 'test_c', sourcefile, currentpath);
asserts.istrue(status, message);
asserts.istrue(isfile(generatedfilename));
txt = fileread(generatedfilename);
asserts.istrue(contains(txt, 'set(module_library_name test_c)'));
rmdir(destinationdir, 's');
%=============================================================================
