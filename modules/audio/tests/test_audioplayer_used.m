%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--AUDIO OUTPUT REQUIRED-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
assert_isequal(length(audioplayer_used()), 0);
r1 = audioplayer(sin(1:10000), 8192);
assert_isequal(length(audioplayer_used()), 1);
r2 = audioplayer(sin(1:10000), 8192);
assert_isequal(length(audioplayer_used()), 2);
delete(r1)
assert_isequal(length(audioplayer_used()), 1);
delete(r2)
assert_isequal(length(audioplayer_used()), 0);
%=============================================================================
