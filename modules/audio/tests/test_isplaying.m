%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--AUDIO OUTPUT REQUIRED-->
%=============================================================================
S = struct('Running', 'on');
asserts.istrue(isplaying(S));
S.Running = 'off';
asserts.isfalse(isplaying(S));
p = audioplayer(zeros(10, 1), 8000);
asserts.isfalse(isplaying(p));
delete(p);
%=============================================================================
