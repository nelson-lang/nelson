%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('deg2rad'), -1);
assert_isequal(nargout('deg2rad'), 1);
%=============================================================================
x = -180;
R = deg2rad(x);
REF = -pi;
assert_isequal(R, REF);
%=============================================================================
x = 180;
R = deg2rad(x);
REF = pi;
assert_isequal(R, REF);
%=============================================================================
