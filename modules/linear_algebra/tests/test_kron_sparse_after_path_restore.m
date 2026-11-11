%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% sparse kron still goes through kron.m after the path and folder are restored
X = speye(3);
asserts.istrue(issparse(kron(X, X)));
savedPath = path();
savedDir = pwd();
cd(fullfile(modulepath('linear_algebra'), 'tests'));
path(savedPath);
cd(savedDir);
R = kron(X, X);
asserts.istrue(issparse(R));
asserts.isequal(full(R), eye(9));
%=============================================================================
