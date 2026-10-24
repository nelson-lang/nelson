%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% single if B or A is single
R = deconv(single([1 2 3]), single([1 1]));
asserts.isequal(class(R), 'single');
R = deconv(single([1 2]), [1 1]);
asserts.isequal(class(R), 'single');
R = deconv([1 2], single([1 1]));
asserts.isequal(class(R), 'single');
[q, r] = deconv(single([1 2]), [1 1]);
asserts.isequal(class(q), 'single');
asserts.isequal(class(r), 'single');
% na > nb branch keeps class rule
[q, r] = deconv(single([1 2]), [1 1 1]);
asserts.isequal(class(q), 'single');
asserts.isequal(class(r), 'single');
asserts.isequal(q, single(0));
asserts.isequal(r, single([1 2]));
% double unchanged
[q, r] = deconv([1 7 16 12], [1 2]);
asserts.isequal(class(q), 'double');
asserts.isequal(q, [1 5 6]);
asserts.isequal(r, [0 0 0 0]);
% scalar divisor
[q, r] = deconv(single([2 4]), single(2));
asserts.isequal(q, single([1 2]));
asserts.isequal(class(r), 'single');
%=============================================================================
