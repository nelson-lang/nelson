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
asserts.isequal(nargin('clusterdata'), -1);
asserts.isequal(nargout('clusterdata'), 1);
%=============================================================================
X = [0 0; 1 0; 0 2; 4 4];
T = clusterdata(X, 'MaxClust', 2);
asserts.isequal(T, [1; 1; 1; 2]);
%=============================================================================
T = clusterdata(X, 'Cutoff', 2, 'Criterion', 'distance');
asserts.isequal(T, [1; 1; 1; 2]);
%=============================================================================
T = clusterdata(X, 'MaxClust', [1 2 3]);
asserts.isequal(T, [1 1 1; 1 1 1; 1 1 2; 1 2 3]);
%=============================================================================
T = clusterdata(X, 'Cutoff', [0.5 2 10], 'Criterion', 'd');
asserts.isequal(T, [1 1 1; 2 1 1; 3 1 1; 4 2 1]);
%=============================================================================
T = clusterdata(X, 'MaxClust', 2, 'Linkage', 'complete');
asserts.isequal(T, [1; 1; 1; 2]);
%=============================================================================
T = clusterdata(X, 'MaxClust', 2, 'Distance', 'cityblock');
asserts.isequal(T, [1; 1; 1; 2]);
%=============================================================================
T = clusterdata(X, 'MaxClust', 2, 'Distance', 'minkowski', 'P', 1.5);
asserts.isequal(T, [1; 1; 1; 2]);
%=============================================================================
T = clusterdata(X, 'MaxClust', 2, 'SaveMemory', 'on', 'Depth', 2);
asserts.isequal(T, [1; 1; 1; 2]);
%=============================================================================
T = clusterdata([0; 1; 10; 11], 'MaxClust', 2);
asserts.isequal(T, [1; 1; 2; 2]);
%=============================================================================
X = [0 0; 0 1; 5 5; 6 5; 9 0];
T = clusterdata(X, 'MaxClust', 3, 'Linkage', 'average');
asserts.isequal(T, [1; 1; 2; 2; 3]);
%=============================================================================
asserts.checkerror('clusterdata()', _('Not enough input arguments.'));
asserts.checkerror('clusterdata({1}, ''MaxClust'', 2)', _('X must be a numeric matrix with at least two rows.'));
asserts.checkerror('clusterdata([1 2], ''MaxClust'', 2)', _('X must be a numeric matrix with at least two rows.'));
asserts.checkerror('clusterdata(ones(2, 2, 2), ''MaxClust'', 2)', _('X must be a numeric matrix with at least two rows.'));
asserts.checkerror('clusterdata(X, ''MaxClust'')', _('Not enough input arguments.'));
asserts.checkerror('clusterdata(X, ''MaxClust'', 2, ''Cutoff'', 1)', _('Specify either Cutoff or MaxClust.'));
asserts.checkerror('clusterdata(X, ''Unknown'', 1, ''MaxClust'', 2)', _('Invalid name-value argument name.'));
asserts.checkerror('clusterdata(X, '''', 1, ''MaxClust'', 2)', _('Invalid name-value argument name.'));
asserts.checkerror('clusterdata(X, ''C'', 1, ''MaxClust'', 2)', _('Ambiguous name-value argument name.'));
asserts.checkerror('clusterdata(X, ''Criterion'', ''bad'', ''MaxClust'', 2)', _('Criterion must be ''distance''.'));
asserts.checkerror('clusterdata(X, ''Criterion'', '''', ''MaxClust'', 2)', _('Criterion must be ''distance''.'));
asserts.checkerror('clusterdata(X, ''SaveMemory'', ''bad'', ''MaxClust'', 2)', _('SaveMemory must be ''on'' or ''off''.'));
asserts.checkerror('clusterdata(X, ''SaveMemory'', '''', ''MaxClust'', 2)', _('SaveMemory must be ''on'' or ''off''.'));
asserts.checkerror('clusterdata(X, ''Distance'', ''minkowski'', ''P'', 0, ''MaxClust'', 2)', sprintf(_('%s must be a positive scalar.'), 'P'));
asserts.checkerror('clusterdata(X, ''MaxClust'', [])', sprintf(_('%s must be numeric.'), 'MaxClust'));
%=============================================================================
