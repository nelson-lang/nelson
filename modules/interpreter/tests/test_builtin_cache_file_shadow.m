%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
% A cached builtin must never hide a path file of the same name.
%=============================================================================
previousDir = pwd();
previousPath = path();
shadowDir = tempname();
mkdir(shadowDir);
filewrite(fullfile(shadowDir, 'issparse.m'), ...
  {'function r = issparse(x)'; '  r = {builtin(''issparse'', x)};'; 'end'});
%=============================================================================
% builtin cached first, then a file of that name comes on the path
asserts.isequal(issparse(1), false);
addpath(shadowDir);
asserts.isequal(issparse(1), {false});
%=============================================================================
% the file calls builtin() of its own name, then the path is restored
cd(fullfile(modulepath('interpreter'), 'tests'));
path(path());
cd(previousDir);
asserts.isequal(issparse(1), {false});
asserts.isequal(feval('issparse', 1), {false});
f = @issparse;
asserts.isequal(f(1), {false});
%=============================================================================
path(previousPath);
asserts.isequal(issparse(1), false);
rmdir(shadowDir, 's');
%=============================================================================
