%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
dlgeneratecleaner(tempdir);
assert_istrue(isfile([tempdir(), 'cleaner.m']));
txt = fileread([tempdir(), 'cleaner.m']);
assert_istrue(contains(txt, 'rmfile(''CMakeLists.txt'');'));
%=============================================================================
