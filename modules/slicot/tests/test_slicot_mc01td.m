%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% http://slicot.org/objects/software/shared/doc/MC01TD.html
% [DP_OUT, STABLE, NZ, IWARN, INFO] = slicot_mc01td(DICO, DP_IN, P)
assert_isequal(nargin('slicot_mc01td'), 3);
assert_isequal(nargout('slicot_mc01td'), 5);
%=============================================================================
DICO = 'C';
DP_IN = 4;
P = [2.0  0.0  1.0  -1.0  1.0];
[DP, STABLE, NZ, IWARN, INFO] = slicot_mc01td(DICO, DP_IN, P);
%=============================================================================
DP_REF = int32(4);
assert_isequal(DP, DP_REF);
%=============================================================================
STABLE_REF = int32(0);
assert_isequal(STABLE, STABLE_REF);
%=============================================================================
NZ_REF = int32(2);
assert_isequal(NZ, NZ_REF);
%=============================================================================
IWARN_REF = int32(0);
assert_isequal(IWARN, IWARN_REF);
%=============================================================================
INFO_REF = int32(0);
assert_isequal(INFO, INFO_REF);
%=============================================================================
