%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(medfilt1([], 3), []);
assert_isequal(size(medfilt1(zeros(0, 2), 3)), [0 2]);
assert_isequal(size(medfilt1(zeros(1, 0), 3)), [1 0]);
assert_isequal(medfilt1(1:5, 3), [1 2 3 4 4]);
asserts.checkerror('sgolay(5, 3)', _('Polynomial order must be less than frame length.'));
asserts.checkerror('sgolayfilt(1:5, 5, 3)', _('Polynomial order must be less than frame length.'));
%=============================================================================
