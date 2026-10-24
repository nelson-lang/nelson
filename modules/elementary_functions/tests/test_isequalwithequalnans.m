%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.istrue(isequalwithequalnans([NaN Inf], [NaN Inf]));
asserts.istrue(isequalwithequalnans(NaN, single(NaN)));
asserts.isfalse(isequalwithequalnans(NaN, 1));
%=============================================================================
