%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [3 1; 1 2; 2 3; 1 1];
asserts.isfalse(issortedrows(A));
asserts.istrue(issortedrows(sortrows(A)));
asserts.istrue(issortedrows([3 2; 2 1; 1 0], 'descend'));
%=============================================================================
