%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('circshift(1:5,true)', _('All values of N must be integer values.'));
asserts.checkerror('circshift(1:5,1,0)', _('DIM must be a positive integer scalar.'));
asserts.isequal(circshift(1:5,1,3), [1 2 3 4 5]);
asserts.isequal(circshift(1:5,[1 1 1]), [5 1 2 3 4]);
asserts.checkerror('circshift(1:5,1i)', _('All values of N must be integer values.'));
asserts.checkerror('circshift(1:5,1,-1)', _('DIM must be a positive integer scalar.'));
asserts.isequal(circshift(magic(3),1,3), reshape([8 3 4 1 5 9 6 7 2], [3 3]));
asserts.isequal(circshift(magic(3),[0 0 1]), reshape([8 3 4 1 5 9 6 7 2], [3 3]));
asserts.isequal(circshift(magic(3),[1 1 1]), reshape([2 6 7 4 8 3 9 1 5], [3 3]));
asserts.isequal(circshift(reshape(1:24,2,3,4),[1 2 3 4]), reshape([10 9 12 11 8 7 16 15 18 17 14 13 22 21 24 23 20 19 4 3 6 5 2 1], [2 3 4]));
asserts.isequal(circshift(reshape(1:24,2,3,4),1,4), reshape([1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24], [2 3 4]));
asserts.isequal(circshift(5,[1 2 3]), 5);
asserts.isequal(circshift(1:5,[0 0 0]), [1 2 3 4 5]);
asserts.isequal(circshift(1:5,[2 0 0]), [1 2 3 4 5]);
asserts.checkerror('circshift(1:5,1,false)', _('DIM must be a positive integer scalar.'));
asserts.checkerror('circshift(1:5,true,2)', _('All values of N must be integer values.'));
asserts.checkerror('circshift(1:5,1,uint8(0))', _('DIM must be a positive integer scalar.'));
%=============================================================================
