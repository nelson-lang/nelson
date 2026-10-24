%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
C = mat2cell(reshape(1:12, [3 4]), [1 2], [3 1]);
asserts.isequal(size(C), [2 2]);
asserts.isequal(C{1, 1}, [1 4 7]);
asserts.isequal(C{2, 2}, [11; 12]);
C = mat2cell(reshape(1:6, [3 2]), [1 2]);
asserts.isequal(size(C), [2 1]);
asserts.isequal(C{1}, [1 4]);
asserts.checkerror('mat2cell(1:4, [1 1])', _('Size arguments must sum to the input size.'));
%=============================================================================
% an empty size vector along an empty dimension
R = mat2cell(zeros(0, 3), [], [1 2]);
asserts.isequal(size(R), [0 2]);
%=============================================================================
