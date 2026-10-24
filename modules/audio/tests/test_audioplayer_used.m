%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--AUDIO OUTPUT REQUIRED-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
skip_testsuite(iswasm(), ...
  'WebAssembly audio backend does not expose the desktop audioplayer_used registry helper.');
%=============================================================================
asserts.isequal(length(audioplayer_used()), 0);
r1 = audioplayer(sin(1:10000), 8192);
asserts.isequal(length(audioplayer_used()), 1);
r2 = audioplayer(sin(1:10000), 8192);
asserts.isequal(length(audioplayer_used()), 2);
delete(r1)
asserts.isequal(length(audioplayer_used()), 1);
delete(r2)
asserts.isequal(length(audioplayer_used()), 0);
%=============================================================================
