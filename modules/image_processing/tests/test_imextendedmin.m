%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('imextendedmin({1}, 1)', sprintf(_('%s must be numeric or logical.'), 'Image'));
%=============================================================================
asserts.checkerror('imextendedmin([1 2], Inf)', sprintf(_('%s must be a nonnegative finite scalar.'), 'Height'));
%=============================================================================
asserts.checkerror('imextendedmin([1 2], 1, eye(3))', _('Unsupported connectivity.'));
