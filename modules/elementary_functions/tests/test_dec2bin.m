%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('dec2bin'), 2)
assert_isequal(nargout('dec2bin'), 1)
%=============================================================================
R = dec2bin(3);
REF = '11';
assert_isequal(R, REF);
%=============================================================================
