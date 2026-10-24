%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% 'nearest': on a tie, the expected behavior takes the right neighbor
asserts.isequal(interp1([1 2], [10 20], 1.5, 'nearest'), 20);
asserts.isequal(interp1([1 2 3], [10 20 30], [1.5 2.5], 'nearest'), [20 30]);
% non-tie cases unchanged
asserts.isequal(interp1([1 2], [10 20], 1.4, 'nearest'), 10);
asserts.isequal(interp1([1 2], [10 20], 1.6, 'nearest'), 20);
asserts.isequal(interp1([1 2], [10 20], 1, 'nearest'), 10);
asserts.isequal(interp1([1 2], [10 20], 2, 'nearest'), 20);
% complex values on a tie
asserts.isequal(interp1([1 2], [10 + 1i 20 + 2i], 1.5, 'nearest'), 20 + 2i);
%=============================================================================
