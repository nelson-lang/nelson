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
% https://github.com/nelson-lang/nelson/issues/869
% <-- Short Description -->
% missing help files in linux package
%=============================================================================
resources_path = [modulepath('help_tools', 'root'), '/resources'];
assert_istrue(isdir(resources_path));
assert_istrue(isfile([resources_path, '/nelson_help_collection.qhc']));
%=============================================================================
