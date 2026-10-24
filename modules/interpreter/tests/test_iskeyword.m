%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
asserts.istrue(iskeyword('for'));
asserts.isfalse(iskeyword('not_a_keyword'));
%=============================================================================
keywords = iskeyword();
asserts.istrue(iscell(keywords));
asserts.istrue(any(strcmp(keywords, 'function')));
asserts.istrue(any(strcmp(keywords, 'end')));
%=============================================================================
