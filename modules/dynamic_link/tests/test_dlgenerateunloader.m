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
dlgenerateunloader(d, 'test_c');
asserts.istrue(isfile(fullfile(d, 'unloader.m')));
txt = fileread(fullfile(d, 'unloader.m'));
asserts.istrue(contains(txt, 'dlclose(lib)'));
%=============================================================================
