%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(sign(true), 1);
asserts.isequal(sign('a'), 1);
asserts.isequal(sign(complex(0,0)), 0);
asserts.isequal(sign(char([0 65])), [0 1]);
asserts.isequal(sign(false), 0);
asserts.isequal(sign(complex(single(0),0)), single(0));
asserts.isequal(sign([true false]), [1 0]);
%=============================================================================
