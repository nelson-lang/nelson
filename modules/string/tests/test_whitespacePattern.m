%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(char(whitespacePattern()), '[\s\x{00A0}\x{2000}-\x{200A}]+');
asserts.isequal(extract("a b", whitespacePattern), " ");
%=============================================================================
