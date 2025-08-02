%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
dlgenerateunloader(tempdir, 'test_c');
assert_istrue(isfile([tempdir(), 'unloader.m']));
txt = fileread([tempdir(), 'unloader.m']);
assert_istrue(contains(txt, 'dlclose(lib)'));
%=============================================================================
