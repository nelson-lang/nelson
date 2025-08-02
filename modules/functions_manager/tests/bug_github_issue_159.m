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
% https://github.com/nelson-lang/nelson/issues/159
% <-- Short Description -->
% addpath must return an warning and not an error for an non existing path.
%=============================================================================
addpath('/never_existing_dir/');
R = lastwarn();
REF = [_('Warning: Not a directory:'), ' ', '/never_existing_dir/', newline];
assert_isequal(R, REF);
