%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
dlgenerateloader(tempdir, 'test_c');
assert_istrue(isfile([tempdir(), 'loader.m']));
txt = fileread([tempdir(), 'loader.m']);
line = 'dlopen([fileparts(nfilename(''fullpathext'')), ''/'', ''test_c'', getdynlibext()]);';
assert_istrue(contains(txt, line));
%=============================================================================
