%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
destinationdir = tempname();
mkdir(destinationdir);
currentpath = fileparts(nfilename('fullpathext'));
sourcefile = [currentpath, '/test_build_c.c'];
ver_comp = version('-compiler');
if strcmp(ver_comp{2}, 'debug')
  expected = 'set(CMAKE_BUILD_TYPE Debug)';
else
  expected = 'set(CMAKE_BUILD_TYPE Release)';
end
%=============================================================================
% empty build configuration gets the default build type line
[status, message, generatedfilename] = dlgeneratemake(destinationdir, 'test_c', sourcefile, currentpath, {}, {}, []);
asserts.istrue(status, message);
txt = fileread(generatedfilename);
asserts.istrue(contains(txt, expected));
%=============================================================================
[status, message, generatedfilename] = dlgeneratemake(destinationdir, 'test_c', sourcefile, currentpath, {}, {}, '');
asserts.istrue(status, message);
txt = fileread(generatedfilename);
asserts.istrue(contains(txt, expected));
%=============================================================================
% omitted build configuration behaves the same
[status, message, generatedfilename] = dlgeneratemake(destinationdir, 'test_c', sourcefile, currentpath);
asserts.istrue(status, message);
txt = fileread(generatedfilename);
asserts.istrue(contains(txt, expected));
%=============================================================================
% explicit build configuration is kept
[status, message, generatedfilename] = dlgeneratemake(destinationdir, 'test_c', ...
  sourcefile, currentpath, {}, {}, 'Debug');
asserts.istrue(status, message);
txt = fileread(generatedfilename);
asserts.istrue(contains(txt, 'set(CMAKE_BUILD_TYPE Debug)'));
asserts.isfalse(contains(txt, 'set(CMAKE_BUILD_TYPE Release)'));
%=============================================================================
% invalid build configuration is rejected
[status, message] = dlgeneratemake(destinationdir, 'test_c', sourcefile, currentpath, {}, {}, 'Fast');
asserts.isfalse(status);
asserts.isequal(message, 'A valid build configuration value expected.');
%=============================================================================
rmdir(destinationdir, 's');
%=============================================================================
