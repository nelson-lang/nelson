%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--PYTHON ENVIRONMENT REQUIRED-->
%=============================================================================
A = pyenv();
asserts.isequal(class(A), 'PythonEnvironment');
%=============================================================================
R = properties(A);
REF = {'Version'; 'Executable'; 'Library'; 'Home'; 'Status'; 'ExecutionMode'};
asserts.isequal(R, REF);
%=============================================================================
asserts.isequal(class(A.Version), 'string');
asserts.isequal(class(A.Executable), 'string');
asserts.isequal(class(A.Library), 'string');
asserts.isequal(class(A.Status), 'string');
asserts.isequal(class(A.ExecutionMode), 'string');
%=============================================================================
asserts.isequal(A.Status, "NotLoaded");
asserts.isequal(A.ExecutionMode, "InProcess");
%=============================================================================
