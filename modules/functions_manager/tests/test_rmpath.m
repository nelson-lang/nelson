%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('rmpath'), -1);
asserts.isequal(nargout('rmpath'), 1);
%=============================================================================
pathTest = tempname();
mkdir(pathTest);
previousPath = path();
addpath(pathTest);
pathWithTest = path();
returnedPath = rmpath(pathTest);
asserts.isequal(returnedPath, pathWithTest);
asserts.isequal(path(), previousPath);
rmdir(pathTest);
%=============================================================================
