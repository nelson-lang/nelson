%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
options = bvpset('RelTol', 1e-5);
asserts.isequal(bvpget(options, 'RelTol'), 1e-5);
% an unset property returns the caller-supplied default, not a baked-in value
asserts.isequal(bvpget(options, 'AbsTol', 1e-7), 1e-7);
asserts.isequal(bvpget(options, 'AbsTol'), []);
asserts.isequal(bvpget(bvpset(), 'NMax', 200), 200);
asserts.checkerror('bvpget(options, ''UnknownOption'')', sprintf(_('Unknown %s option name.'), 'BVP'));
%=============================================================================
