%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% identity
assert_isequal(makehgtform(), eye(4));
%=============================================================================
% translation
T = makehgtform('translate', [2 3 4]);
assert_isequal(T(1:3, 4)', [2 3 4]);
assert_isequal(T * [0; 0; 0; 1], [2; 3; 4; 1]);
%=============================================================================
% scale (vector and scalar)
assert_isequal(makehgtform('scale', [2 3 4]), diag([2 3 4 1]));
assert_isequal(makehgtform('scale', 5), diag([5 5 5 1]));
%=============================================================================
% rotations (right-handed)
Rz = makehgtform('zrotate', pi / 2);
assert_istrue(norm(Rz * [1; 0; 0; 1] - [0; 1; 0; 1]) < 1e-12);
Rx = makehgtform('xrotate', pi / 2);
assert_istrue(norm(Rx * [0; 1; 0; 1] - [0; 0; 1; 1]) < 1e-12);
Ry = makehgtform('yrotate', pi / 2);
assert_istrue(norm(Ry * [0; 0; 1; 1] - [1; 0; 0; 1]) < 1e-12);
%=============================================================================
% composition is left-to-right matrix product (first listed applied last)
M = makehgtform('translate', [1 0 0], 'scale', [2 2 2]);
assert_isequal(M, makehgtform('translate', [1 0 0]) * makehgtform('scale', [2 2 2]));
assert_istrue(norm(M * [1; 0; 0; 1] - [3; 0; 0; 1]) < 1e-12);
%=============================================================================
% errors
assert_checkerror('makehgtform(''zrotate'')', _('Missing value for transformation ''zrotate''.'));
assert_checkerror('makehgtform(''bogus'', 1)', _('Unknown transformation ''bogus''.'));
assert_checkerror('makehgtform(''translate'', [1 2])', _('Translation must be a three-element vector.'));
%=============================================================================
