%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% The default streamline integration must run long enough (max vertices 10000)
% so a streamline that stays inside the grid is not truncated prematurely.
% Regression: the default used to stop at 500 vertices, giving streamlines that
% were far shorter than expected for a bounded rotational field.
%=============================================================================
[x, y] = meshgrid(-2:0.25:2, -2:0.25:2);
vertices = stream2(x, y, -y, x, 1, 0);
asserts.isequal(numel(vertices), 1);
line = vertices{1};
asserts.istrue(size(line, 1) > 2000);
%=============================================================================
% The explicit [stepsize, maxvert] option still caps the length.
verticesCapped = stream2(x, y, -y, x, 1, 0, [0.1 300]);
asserts.istrue(size(verticesCapped{1}, 1) <= 300);
%=============================================================================
