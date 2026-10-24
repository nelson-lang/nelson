%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
d = tempname();
mkdir(d);
c = onCleanup(@() rmdir(d, 's')); %#ok<NASGU>
dlgenerateloader(d, 'test_c');
asserts.istrue(isfile(fullfile(d, 'loader.m')));
txt = fileread(fullfile(d, 'loader.m'));
line = 'dlopen([fileparts(nfilename(''fullpathext'')), ''/'', ''test_c'', getdynlibext()]);';
asserts.istrue(contains(txt, line));
%=============================================================================
