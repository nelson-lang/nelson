%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/691
% <-- Short Description -->
% help generation crashs if copyright tag is empty
%=============================================================================
currentpath = fileparts(nfilename('fullpathext'));
status = xmldoctohelp([currentpath, '/xml'], tempdir(), 'test 691', true);
assert_istrue(isfile(status));
%=============================================================================
