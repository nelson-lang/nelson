%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('streamline(ones(2), ones(3), 1, 1);', getString(message('Nelson:dimagree:vectorComponentSizesMatch')));
asserts.checkerror('streamline(ones(2), ones(2), [1 2], 1);', getString(message('Nelson:streamlineBase:invalidStartPointArrays')));
%=============================================================================
