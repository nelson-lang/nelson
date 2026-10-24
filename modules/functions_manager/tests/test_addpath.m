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
asserts.isequal(nargin('addpath'), -1);
asserts.isequal(nargout('addpath'), 1);
%=============================================================================
pathTest = tempname();
mkdir(pathTest);
previousPath = path();
returnedPath = addpath(pathTest);
asserts.isequal(returnedPath, previousPath);
asserts.istrue(contains(path(), pathTest));
rmpath(pathTest);
asserts.isequal(path(), previousPath);
rmdir(pathTest);
%=============================================================================
root = tempname();
mkdir(root);
pathOne = fullfile(root, 'one');
pathTwo = fullfile(root, 'two');
mkdir(pathOne);
mkdir(pathTwo);
pathOne = fullpath(pathOne);
pathTwo = fullpath(pathTwo);
previousPath = path();
returnedPath = addpath([pathOne, pathsep(), pathTwo, pathsep()]);
asserts.isequal(returnedPath, previousPath);
asserts.istrue(contains(path(), pathOne));
asserts.istrue(contains(path(), pathTwo));
rmpath(pathOne);
rmpath(pathTwo);
asserts.isequal(path(), previousPath);
rmdir(pathOne);
rmdir(pathTwo);
rmdir(root);
%=============================================================================
