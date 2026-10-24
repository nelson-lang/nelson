%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = evalc('__display_expandable_text__(''More'', ''Hidden'', [''Fallback'', char(10)]);');
asserts.isequal(R, ['Fallback', char(10)]);
%=============================================================================
handled = __display_expandable_text__('More', 'Hidden', '');
asserts.isfalse(handled);
%=============================================================================
