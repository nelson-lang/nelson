%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
[names, paths, versions, protected] = getmodules();
%=============================================================================
asserts.istrue(iscellstr(names));
asserts.istrue(iscellstr(paths));
asserts.istrue(iscell(versions));
asserts.istrue(islogical(protected));
asserts.isequal(length(names), length(paths));
asserts.isequal(length(names), length(versions));
asserts.isequal(length(names), length(protected));
asserts.istrue(any(strcmp(names, 'core')));
%=============================================================================
