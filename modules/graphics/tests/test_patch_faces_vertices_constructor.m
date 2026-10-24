%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
figure();
v = [0 0; 1 0; 1 1; 0 1];
f = [1 2 3 4];
p = patch('Faces', f, 'Vertices', v);
%=============================================================================
% Interp/flat face colouring with no FaceVertexCData must raise a clear error
% rather than crash (empty colour buffer was dereferenced).
v3 = [0 0 0; 1 0 0; 1 1 0; 0 1 0];
pc = patch('Faces', [1 2 3 4], 'Vertices', v3, 'FaceColor', [1 0 0]);
asserts.checkerror('set(pc, ''FaceColor'', ''interp'')', ...
  'Invalid FaceVertexCData parameter with FaceColor to ''interp''.');
delete(pc);
% With valid per-vertex colour data it works
pd = patch('Faces', [1 2 3 4], 'Vertices', v3, 'FaceVertexCData', [1; 2; 3; 4], 'FaceColor', 'interp');
asserts.isequal(char(pd.FaceColor), 'interp');
%=============================================================================
