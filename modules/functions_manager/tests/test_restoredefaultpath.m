%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
u1 = userpath();
%=============================================================================
restoredefaultpath();
%=============================================================================
u2 = userpath();
if ~(strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm())
  asserts.isequal(u1, u2);
end
%=============================================================================
REF = path();
path('');
P1 = path();
u3 = userpath();
if ~(strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm())
  asserts.isequal(u1, u2);
end
asserts.isequal(P1, '');
%=============================================================================
restoredefaultpath();
u3 = userpath();
P2 = path();
if ~(strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm())
  asserts.isequal(u1, u3);
end
asserts.isequal(REF, P2);
%=============================================================================
