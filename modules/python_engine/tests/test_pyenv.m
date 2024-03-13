%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = pyenv();
assert_isequal(class(A), 'PythonEnvironment');
%=============================================================================
R = properties(A);
REF = {'Version'; 'Executable'; 'Library'; 'Home'; 'Status'; 'ExecutionMode'};
assert_isequal(R, REF);
%=============================================================================
assert_isequal(class(A.Version), 'string');
assert_isequal(class(A.Executable), 'string');
assert_isequal(class(A.Library), 'string');
assert_isequal(class(A.Status), 'double');
assert_isequal(class(A.ExecutionMode), 'double');
%=============================================================================