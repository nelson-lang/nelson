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
%=============================================================================
asserts.isequal(nargin('cluster'), -1);
asserts.isequal(nargout('cluster'), 1);
%=============================================================================
X = [0 0; 1 0; 0 2; 4 4];
Z = linkage(X);
T = cluster(Z, 'MaxClust', 2);
asserts.isequal(T, [1; 1; 1; 2]);
%=============================================================================
T = cluster(Z, 'MaxClust', [1 2 3]);
asserts.isequal(T, [1 1 1; 1 1 1; 1 1 2; 1 2 3]);
%=============================================================================
T = cluster(Z, 'Cutoff', 2, 'Criterion', 'distance');
asserts.isequal(T, [1; 1; 1; 2]);
%=============================================================================
T = cluster(Z, 'Cutoff', [0.5 2 10], 'Criterion', 'distance');
asserts.isequal(T, [1 1 1; 2 1 1; 3 1 1; 4 2 1]);
%=============================================================================
asserts.checkerror('cluster()', _('Not enough input arguments.'));
asserts.checkerror('cluster([1 2; 3 4], ''MaxClust'', 2)', _('Z must have three columns.'));
asserts.checkerror('cluster(Z, ''MaxClust'')', _('Not enough input arguments.'));
asserts.checkerror('cluster(Z, ''Unknown'', 2)', _('Invalid option name.'));
asserts.checkerror('cluster(Z, ''Criterion'', ''bad'', ''MaxClust'', 2)', _('Criterion must be ''distance''.'));
asserts.checkerror('cluster(Z, ''MaxClust'', 2, ''Cutoff'', 1)', _('Specify either Cutoff or MaxClust.'));
%=============================================================================
