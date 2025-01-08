%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
userpath('reset');
up1 = userpath();
userpath(tempdir);
up2 = userpath();
R = replace(tempdir(), ['/', pathsep], pathsep);
assert_isequal(R, [up2, '/']);
userpath('clear');
up3 = userpath();
assert_isequal('', up3);
userpath('reset');
up4 = userpath();
assert_isequal(up1, up4);
userpath('reset');
userpath();
%=============================================================================
